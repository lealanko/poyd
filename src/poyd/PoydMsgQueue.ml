
module M = BatMutex.Mutex
module Q = Queue
module E = Event

type 'a t = {
    queue : 'a Q.t;
    mutex : M.t;
    mutable waiters : int;
    chan : 'a E.channel;
}


let create () = {
    queue = Q.create ();
    mutex = M.create ();
    chan = E.new_channel ();
    waiters = 0;
}


let locked q thunk =
    M.synchronize ~lock:q.mutex thunk ()

let send q x = 
    let waiting = locked q (fun () -> 
        if q.waiters > 0 then begin
            q.waiters <- q.waiters - 1;
            true
        end else begin
            Q.add x q.queue;
            false
        end) in
    if waiting then
        E.sync (E.send q.chan x)

let receive q =
    E.guard (fun () ->
        let got = locked q (fun () ->
            try Some (Q.take q.queue)
            with Q.Empty ->
                q.waiters <- q.waiters + 1;
                None) in
        match got with
        | Some x -> E.wrap_abort (E.always x) (fun () -> send q x)
        | None -> E.receive q.chan)
        
    
