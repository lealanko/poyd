open NcPrelude
include TaskMgrDefs

module L = (val Log.make "TaskMgr" : Log.S)

type state = 
    | Running
    | Closed

module Make(Arg : UNIT) = struct
    let state = ref Running

    let cancels = Seq.create ()

    let finish, finish_waker = task ()
    let notify_finish = 
        let l = lazy (wakeup finish_waker ()) in
        fun () -> Lazy.force l

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

let _ = ExnMapper.register MgrClosed
