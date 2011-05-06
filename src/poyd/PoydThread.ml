open PoydPrelude

module E = Event
module MQ = PoydMsgQueue


type t = {
    mutable thread : Thread.t;
    task_mq : (unit -> unit) MQ.t;
}

let lwt_task_chan = E.new_channel ()

let handle_lwt_task () =
    let task = E.sync (E.receive lwt_task_chan) in
    detach task

let lwt_task_notify_id = Lwt_unix.make_notification handle_lwt_task

let callback (type r) thr f =
    let ret_chan = E.new_channel () in
    let task () =
        result f >>= fun res ->
        E.sync (E.send ret_chan (`Return res));
        return () in
    Lwt_unix.send_notification lwt_task_notify_id;
    E.sync (E.send lwt_task_chan task);
    let rec poll () =
        match E.select [
            E.receive ret_chan; 
            E.wrap (MQ.receive thr.task_mq) (fun f -> `Task f)
        ] with
        | `Return r -> BatStd.ok r
        | `Task f -> f (); poll ()
    in
    poll ()

let rec thread_main thr =
    let f = E.sync (MQ.receive thr.task_mq) in
    f ();
    thread_main thr
            

let run thr thunk =
    ccc (fun k ->
        let ret_chan = E.new_channel () in
        let notify_id = Lwt_unix.make_notification ~once:true (fun () -> 
            let res = E.sync (E.receive ret_chan) in
            wakeup_result k res) in
        let task () =
            let ret = BatStd.wrap thunk () in
            Lwt_unix.send_notification notify_id;
            E.sync (E.send ret_chan ret)
        in
        MQ.send thr.task_mq task)

let create () = 
    let thr = {
        thread = Thread.self ();
        task_mq = MQ.create ();
            
    } in
    thr.thread <- Thread.create (fun () -> thread_main thr) ();
    thr
