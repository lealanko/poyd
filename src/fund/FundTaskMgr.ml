open FundPrelude
include FundTaskMgrDefs

module L = (val FundLog.make "TaskMgr" : FundLog.S)

type state = 
    | Running
    | Closed

module Make(Arg : UNIT) = struct
    let state = ref Running

    let cancels = Seq.create ()

    let finish_cond = Lwt_condition.create ()

    let finish = Lwt_condition.wait finish_cond

    let notify_finish () = 
        Lwt_condition.broadcast finish_cond ()

    let finish_task node =
        Seq.remove node;
        if !state = Closed && Seq.is_empty cancels then 
            notify_finish ()

    let task thunk = 
        if !state = Closed then
            fail MgrClosed
        else 
            fix (fun t ->
                let node = Seq.push cancels (fun () -> cancel t) in
                finalize (thunk)
                    (fun () -> return (finish_task node)))

    let close () = 
        L.dbg "close" >>= fun () ->
        state := Closed;
        if Seq.is_empty cancels then 
            notify_finish ();
        return ()
            
    let abort () =
        L.dbg "abort" >>= fun () ->
        state := Closed;
        Seq.iter_s (fun c ->
            L.dbg "canceling" >>= fun () ->
            c ();
            return ()) cancels >>= fun () ->
        notify_finish ();
        return ()

    let _ = on_cancel finish (fun () -> detach abort)

end

let _ = FundExnMapper.register MgrClosed
