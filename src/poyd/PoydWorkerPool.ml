open PoydPrelude

module L = (val FundLog.make "PoydWorkerPool" : FundLog.S)

type worker = PoydServantStub.t

type consumer = {
    priority : int;
    mutable cont : worker cont option;
}

module C = struct
    type t = consumer
    let compare c1 c2 = BatInt.compare c1.priority c2.priority
end

module Q = BatHeap.Make(C)

type t = {
    mutable available : worker BatSet.t;
    mutable pending_consumers : Q.t;
}

let create () = {
    available = BatSet.empty;
    pending_consumers = Q.empty;
}

let get p pri =
    try
        let w, rest = BatSet.pop p.available in
        p.available <- rest;
        L.dbg "Had available, taking directly, now have %d" 
            (BatSet.cardinal p.available) >>= fun () ->
        return w
    with Not_found ->
        let t = ccc (fun k ->
            let con = { priority = pri; cont = Some k } in
            on_cancel_w k (fun () -> 
                (* Printf.eprintf "Get request canceled\n%!"; *)
                con.cont <- None);
            p.pending_consumers <- Q.insert p.pending_consumers con)
        in
        L.dbg "Didn't have available, now pending" >>= fun () ->
        t

let rec put p w = 
    if Q.size p.pending_consumers = 0 then begin
        p.available <- BatSet.add w p.available;
        L.dbg "No consumers, adding to available, now have %d" 
            (BatSet.cardinal p.available)
    end else begin
        let con = Q.find_min p.pending_consumers in
        p.pending_consumers <- Q.del_min p.pending_consumers;
        match con.cont with
        | None -> begin
            L.dbg "Found canceled consumer, retry" >>= fun () ->
            put p w
        end
        | Some k -> begin 
            L.dbg "Found live consumer, giving directly" >>= fun () ->
            Lwt.wakeup k w;
            return ()
        end
    end
        
        

