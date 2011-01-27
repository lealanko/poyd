open NcPrelude
include TaskMgrDefs

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
        Seq.get node (); (* Cancel, or nop if thread's no longer running *)
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
        state := Closed
            
    let abort () =
        state := Closed;
        Seq.iter_node_l finish_task cancels

    let _ = on_cancel finish abort

end

let _ = ExnMapper.register MgrClosed
