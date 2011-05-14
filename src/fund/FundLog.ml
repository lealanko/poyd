open FundPrelude

type ftr = Format.formatter



module type S = sig
    val dbg : ('a, ftr, unit, unit lwt) format4 -> 'a
    val info : ('a, ftr, unit, unit lwt) format4 -> 'a
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

let template = "$(message)"

let default_logger = 
    Lwt_log.channel ~template ~close_mode:`Keep ~channel:Lwt_io.stderr ()

let make ?(logger=default_logger) secname = (module struct
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

    let log ~level fmt = 
        ksprintf fmt (fun s -> Lwt_log.log ~section ~level ~logger s)

    let dbg fmt = log ~level:Lwt_log.Debug fmt
    let info fmt = log ~level:Lwt_log.Info fmt

    let trace ?(pr=pr_any) thunk fmt = 
        ksprintf fmt (fun s ->
            Lwt_log.debug_f ~section ~logger "-> %s" s >>= fun () ->
            try_bind thunk
                (fun r ->
                    Lwt_log.debug_f ~section ~logger "@[@]<- %s: %s" s 
                        (prs (fun f -> pr f r)) >>= fun () ->
                    return r)
                (fun exn ->
                    Lwt_log.debug_f ~section ~logger ~exn "@[@]<# %s" s 
                    >>= fun () ->
                    fail exn))

    let trace2 ?pr ?(p1=pr_any) ?(p2=pr_any) s f a1 a2 =
        trace ?pr (fun () -> f a1 a2) "@[%s %a %a@]" s p1 a1 p2 a2
        
        
end : S)
            
