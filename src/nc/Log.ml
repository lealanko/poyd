open NcPrelude

module type S = sig
    val dbg : ('a, Format.formatter, unit, unit lwt) format4 -> 'a
    val trace : (unit -> 'a lwt) -> ?pr:(Format.formatter -> 'a -> unit) ->
        ('b, Format.formatter, unit, 'a lwt) format4 -> 'b
end

type t = (module S)

let make secname = (module struct
    let section = Lwt_log.Section.make secname


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
            
    let pr_any f v =
        Format.fprintf f "@[%s@]" (BatStd.dump v)

    let dbg fmt = 
        ksprintf fmt (fun s -> Lwt_log.debug ~section s)

    let trace thunk ?(pr=pr_any) fmt = 
        ksprintf fmt (fun s ->
            Lwt_log.debug_f ~section "-> %s" s >>= fun () ->
            try_bind thunk
                (fun r ->
                    Lwt_log.debug_f ~section "<- %s: %s" s (prs (fun f -> pr f r))
                    >>= fun () ->
                    return r)
                (fun exn ->
                    Lwt_log.debug_f ~section ~exn "<# %s" s >>= fun () ->
                    fail exn))
        
end : S)
            
