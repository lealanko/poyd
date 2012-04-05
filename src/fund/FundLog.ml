open FundPrelude

type ftr = Format.formatter



module type S = sig
    val dbg : ('a, ftr, unit, unit lwt) format4 -> 'a
    val info : ('a, ftr, unit, unit lwt) format4 -> 'a
    val error : ('a, ftr, unit, unit lwt) format4 -> 'a
    val trace : ?pr:(ftr -> 'a -> unit) -> (unit -> 'a lwt) ->
        ('b, ftr, unit, 'a lwt) format4 -> 'b
    val trace2 : 
        ?pr:(ftr -> 'r -> unit) ->
        ?p1:(ftr -> 'a -> unit) ->
        ?p2:(ftr -> 'b -> unit) ->
        string ->
        ('a -> 'b -> 'r lwt) ->
        'a -> 'b -> 'r lwt

end

type t = (module S)

let kprs f = 
    let buf = Buffer.create 16 in
    let formatter = Format.formatter_of_buffer buf in
    f formatter (fun k ->
        Format.pp_print_flush formatter ();
        k (Buffer.contents buf))
        
let prs f =
    kprs (fun formatter k -> f formatter; k identity)

let ksprintf fmt k =
    kprs (fun formatter k2 ->
        Format.kfprintf (fun _ -> k2 k) formatter fmt)

let dump (f : Format.formatter) (v : 'a) : unit  =
  let p fmt = Format.fprintf f fmt
  in
  let w s = Format.pp_print_string f s
  in
  let hexchar c = p "%02x" (Char.code c)
  in
  let hexstring s = String.iter hexchar s
  in
  let ht = Hashtbl.create 0
  in
  let rec dump r =
  if Obj.is_int r then
    w (string_of_int (Obj.magic r : int))
  else
    dump_block r
  and dump_block r =
    let rec get_fields acc = function
      | 0 -> acc
      | n -> let n = n-1 in get_fields (Obj.field r n :: acc) n
    in
    let rec is_list r =
      if Obj.is_int r then
	r = Obj.repr 0 (* [] *)
      else
	let s = Obj.size r and t = Obj.tag r in
	t = 0 && s = 2 && is_list (Obj.field r 1) (* h :: t *)
    in
    let rec get_list r =
      if Obj.is_int r then
	[]
      else
	let h = Obj.field r 0 and t = get_list (Obj.field r 1) in
	h :: t
    in
    let opaque name x =
      match wrap (Hashtbl.find ht) x with
        | Ok id ->
          p "<%s:%s>" name (Digest.to_hex id)
        | Bad _ ->
          match wrap (Marshal.to_string x) [] with
            | Ok s -> 
              let id = Digest.string s
              in
              Hashtbl.add ht x id;
              p "<%s:%s:%d:" name (Digest.to_hex id) (String.length s);
              hexstring (String.sub s 0 (min (String.length s) 256));
              w ">"
            | Bad _ ->
              p "<%s>" name
    in
    let rec dump_list sep l =
      match l with 
        | [] -> ()
        | [e] -> dump e
        | (h::t) -> dump h; w sep; dump_list sep t
    in
    let s = Obj.size r and t = Obj.tag r in
    (* From the tag, determine the type of block. *)
    match t with
      | _ when is_list r ->
	let fields = get_list r in
        w "[";
        dump_list "; " fields;
        w "]"
      | 0 ->
	let fields = get_fields [] s in
        w "(";
        dump_list ", " fields;
        w ")"
      | x when x = Obj.lazy_tag ->
	(* Note that [lazy_tag .. forward_tag] are < no_scan_tag.  Not
	 * clear if very large constructed values could have the same
	 * tag. XXX *)
	opaque "lazy" r
      | x when x = Obj.closure_tag ->
	opaque "closure" r
      | x when x = Obj.object_tag ->
	let fields = get_fields [] s in
	let _clasz, id, slots =
	  match fields with
	    | h::h'::t -> h, h', t
	    | _ -> assert false
	in
	(* No information on decoding the class (first field).  So just print
	 * out the ID and the slots. *)
        w "Object #";
        dump id;
        w " (";
        dump_list ", " slots;
        w ")";
      | x when x = Obj.infix_tag ->
	opaque "infix" r
      | x when x = Obj.forward_tag ->
	opaque "forward" r
      | x when x < Obj.no_scan_tag ->
	let fields = get_fields [] s in
        w "Tag";
        w (string_of_int t);
        w "(";
        dump_list ", " fields;
        w ")"
      | x when x = Obj.string_tag ->
        w "\"";
	w (String.escaped (Obj.magic r : string));
        w "\""
      | x when x = Obj.double_tag ->
	w (string_of_float (Obj.magic r : float))
      | x when x = Obj.abstract_tag ->
	opaque "abstract" r
      | x when x = Obj.custom_tag ->
	opaque "custom" r
      | x when x = Obj.final_tag ->
	opaque "final" r
      | x when x = Obj.double_array_tag ->
	w (BatIO.to_string (BatArray.print BatFloat.print) (Obj.magic r : float array))
      | _ ->
	opaque (Printf.sprintf "unknown: tag %d size %d" t s) r
  in
  dump (Obj.repr v)

let pr_any (f : Format.formatter) (v : 'a) : unit =
  Format.fprintf f "@[";
  dump f v;
  Format.fprintf f "@]"
        

let template = "$(message)"

let default_logger = 
    Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stderr ()

let make ?(logger=default_logger) secname = (module struct
    let section = Lwt_log.Section.make secname

    let log ~level fmt = 
        ksprintf fmt (fun s -> Lwt_log.log ~section ~level ~logger s)

    let dbg fmt = log ~level:Lwt_log.Debug fmt
    let info fmt = log ~level:Lwt_log.Info fmt
    let error fmt = log ~level:Lwt_log.Error fmt

    let trace ?(pr=pr_any) thunk fmt = 
        ksprintf fmt (fun s ->
            Lwt_log.debug_f ~section ~logger "-> %s" s >>= fun () ->
            try_bind thunk
                (fun r ->
                    Lwt_log.debug_f ~section ~logger "<- %s: %s" s 
                        (prs (fun f -> pr f r)) >>= fun () ->
                    return r)
                (fun exn ->
                    Lwt_log.debug_f ~section ~logger ~exn "<# %s" s 
                    >>= fun () ->
                    fail exn))

    let trace2 ?pr ?(p1=pr_any) ?(p2=pr_any) s f a1 a2 =
        trace ?pr (fun () -> f a1 a2) "%s %a %a" s p1 a1 p2 a2
        
        
end : S)
            
