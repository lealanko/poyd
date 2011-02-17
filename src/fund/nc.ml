
module Prelude = struct

    include Lwt
    include Lwt_io

    type 'a lwt = 'a Lwt.t

    let (<&) a b =
	let (t, u) = task () in
	let r = a >|= wakeup u in
	r <&> b >>= fun () -> t
	
	
    type ('a, 'b) either = 
	| Left of 'a
	| Right of 'b

    let (<|>) a b =
	(a >|= fun x -> Left x) 
	<?> (b >|= fun y -> Right y)

    let (<?) a b =
	(a >|= fun x -> Some x)
	<?> (b >|= fun () -> None)

    let const v = fun _ -> v

    let uconnect t u =
	ignore (try_bind (fun () -> t)
		    (fun v -> return (wakeup u v))
		    (fun exn -> return (wakeup_exn u exn)))

    let protect t =
	let tt, tu = task () in
	uconnect t tu;
	tt

    let no_cancel t =
	let wt, wu = wait () in
	uconnect t wu;
	wt
	    
    let block t =
	protect t <&> no_cancel t


    let detach thunk =
	ignore (apply thunk ())

end



open Prelude

(*
module type EXC_WRAPPER = sig
    type e
    exception Wrap of e
end

type 'e exc_wrapper = (module EXC_WRAPPER with type e = 'e)

let mkEW (type ee) () : ee exc_wrapper =
    let module E = struct
	type e = ee
	exception Wrap of e
    end
    in (module E : EXC_WRAPPER with type e = ee)

*)
(*
type 'e exw = { 
    wrap : 'e -> exn;
    unwrap : exn -> 'e option
}


let mkExcWrapper (type e) () : e -> exn * (exn -> e option) =
    let module E = struct
	exception Wrap of e
    end in
    (fun e -> E.Wrap e, function E.Wrap e -> Some e | _ -> None)

*)
module type HANDLE = sig
    type ('a,'r) t

    val create : ('a -> 'r lwt) -> ('a, 'r) t

    val apply : ('a, 'r) t -> 'a -> 'r lwt
end


module Handle : HANDLE = struct

    type ('a, 'r) t = int

    type ('a, 'r) stub = {
	key : ('a, 'r) t;
	impl : 'a -> 'r lwt
    }

    module M : sig
	val mk : int -> ('a, 'r) t
	val un : ('a, 'r) t -> int
    end = struct
	let mk i = i
	let un i = i
    end

    module W = Weaktbl.MakeDef(struct type t = int end)

    let table : Obj.t W.t = W.create 19

    let next_key : int ref = ref 0

    let apply (h : ('a, 'r) t) (v : 'a) : 'r lwt =
	let stub = Obj.obj (W.get table h) in
	stub.impl v

    let create impl =
	let key (type a) = !next_key in
	next_key := key + 1;
	let stub = { key; impl } in
	W.set table key (Obj.repr stub);
	key
end
	


module type CONNECTION = sig
    type t
    val connect : Unix.sock_addr -> t
    val get_root : string -> 'a

    type listener
    val listen : Unix.sock_addr -> listener
    val set_root : listener -> string -> 'a -> unit
    val shutdown : listener -> unit
end

module Connection : CONNECTION = struct
    type key = Uuidm.t
	    
    let local_key : key =
	Uuidm.create `V4

    module IO = Lwt_io

    type rqid = int


    module W = Weaktbl.MakeDef(struct type t = key end)

    let table : t W.t = 
	W.create ()
	    
    type request = [
      | `IAm of key
      | `Apply of Handle.t * Obj.t
      | `Ping
      | `Close
      | `Cancel of rqid
      | `Root of string
    ]

    type response = [
      | `OK
      | `Error
      | `Return of Obj.t
    ]

    exception Closed

    type t = {
	in_ch : IO.input_channel;
	out_ch : IO.output_channel;
	mutable remote_key : key option;
	read_loop : unit lwt;
	halt_sig : unit Lwt_condition.t;
	mutable next_rqid : reqid;
	pending_remote : (reqid, response lwt * response Lwt.u) Hashtbl.t;
	pending_local : (reqid, unit lwt) Hashtbl.t;
    }

    let write_msg conn (msg : message) =
	(* Lwt_io.write_value is atomic *)
	write_value conn.out_ch msg

    let read_msg conn : message lwt =
	read_value conn.in_chlw


    type message =
	| Request of rqid * request
	| Response of rqid * response

    exception Closing

    let request conn rq =
	if conn.closing 
	then fail Closing
	else
	let rqid = conn.next_rqid in
	conn.next_rqid <- rqid + 1;
	let t, u = task () in
	finalize (fun () ->
	    ccc (fun k ->
		Hashtbl.add conn.pending_rqs rqid k;
		write_msg conn (Request rq) >>= fun () ->
	t

    let hash_values ht =
	Hashtbl.fold (fun k v l -> v :: l) ht []


    let shutdown conn =
	close conn.in_ch;
	close conn.out_ch

    let do_close conn =
	let t, u = 
	let closer =
	    let locals = hash_values conn.pending_locals
	    and remotes = List.map (fun (t,u) -> t >|= fun _ -> ())
		(hash_values conn.pending_remotes)
	    in
	    join (locals @ remotes)
	in
	shutdown conn

    let close conn =
	if not conn.closing then begin
	    request conn `Close >>= fun _ ->
	    do_close conn
	end


    let handle_request conn rq = : request -> response lwt = function
	| `Close -> 
	    detach (fun () -> do_close conn);
	    return `OK
	| _ when conn.closing -> 
	    return `Error
	| `IAm remote_key -> begin 
	    match conn.remote_key with
	    | None -> 
		conn.remote_key <- Some remote_key;
		return `OK
	    | Some _ ->
		return `Error
	end
	| `Apply h v ->
	    Handle.apply h v >|= `Return
	| `Ping -> 
	    return `Pong
	| `Cancel rqid -> 
	    let t = W.get conn.pending_local rqid in
	    match state t with
	    | Sleeping -> begin
		cancel t;
		match state t with
		| Fail Canceled -> `OK
		| _ -> `Error
	    end
	    | _ -> `Error
	
    let handle_message conn = function
	| Response rqid resp ->
	    let p = conn.pending_rqs in
	    if Hashtbl.mem p rqid then 
		let u = Hashtbl.find p rqid in
		Hashtbl.remove p rqid;
		wakeup u rqid
	| Request rqid rq ->
	    ignore (fix (fun t ->
		(* We must register this thread before handling the
		   request, because if the request is `Close it
		   might not realize that it has to wait until we
		   have sent the response before shutting down. *)
		Hashtbl.set conn.pending_locals rqid t;
		finalize (fun () ->
		    handle_request conn rq >>= fun resp ->
		    write_msg conn (Response rqid resp))
		    (fun () -> Hashtbl.remove conn.pending_locals rqid)))


    let read_loop conn =
	read_msg conn >>= fun msg ->
	handle_message conn msg;
	read_loop conn
	    
end

