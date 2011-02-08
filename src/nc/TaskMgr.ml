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

    let finish_task do_cancel node =
        Seq.remove node;
        let cancel = Seq.get node in
        if do_cancel then cancel ();
        if !state = Closed && Seq.is_empty cancels then 
            notify_finish ()

    let task thunk = 
        if !state = Closed then
            fail MgrClosed
        else 
            fix (fun t ->
                let node = Seq.push cancels (fun () -> cancel t) in
                finalize (thunk)
                    (fun () -> return (finish_task false node)))

    let close () = 
        L.dbg "close" >>= fun () ->
        state := Closed;
        return ()
            
    let abort () =
        L.dbg "abort" >>= fun () ->
        state := Closed;
        Seq.iter_node_l (finish_task true) cancels;
        return ()


    let _ = on_cancel finish (fun () -> detach abort)

end

let _ = ExnMapper.register MgrClosed
