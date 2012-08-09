open PoydPrelude


let thread = PoydThread.create ()

let _ = FundExnMapper.register (MainUtil.ExitPoy 0)


module L = Lwt_log
module S = Status


let convert_level = function
    | L.Debug 
    | L.Info -> S.Information
    | L.Notice -> S.Status
    | L.Warning -> S.Warning
    | L.Error
    | L.Fatal -> S.Error

let status_logger =
    let output section level lines =
        let c = convert_level level 
        in
        (* PoydThread.run thread (fun () -> *)
        List.iter (fun line ->
            Status.user_message c (StatusCommon.escape line)) lines;
        return ()
    (* ) *)
    in
    let close () = return ()
    in
    L.make ~output ~close

    
