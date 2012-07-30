open PoydPrelude

module L = (val FundLog.make "PoydWorkerPool" : FundLog.S)

type worker = PoydServantStub.t

module Q = struct
    module M = BatMap
    module S = Lwt_sequence
    type ('a, 'b) t = {
        mutable map : ('a, 'b S.t) M.t;
        mutable size : int;
    }
    type ('a, 'b) node = {
        node : 'b S.node;
        key : 'a;
        seq : 'b S.t;
        heap : ('a, 'b) t;
    }
    let get_list k t =
        try M.find k t.map 
        with Not_found ->
            let l = S.create ()
            in
            t.map <- M.add k l t.map;
            l

    let insert key value heap = 
        let seq = get_list key heap
        in
        heap.size <- heap.size + 1;
        let node = S.add_r value seq
        in
        { node; key; seq; heap }

    let size heap = heap.size

    let create () = {
        map = M.empty;
        size = 0;
    }

    let delete { node; key; seq; heap } =
        S.remove node;
        let seq_ = get_list key heap
        in
        if seq_ == seq then begin
            heap.size <- heap.size - 1;
            if S.is_empty seq_ then
                heap.map <- M.remove key heap.map
        end
            
    let take_min heap =
        let (key, seq) = M.min_binding heap.map
        in
        let v = S.take_l seq
        in
        heap.size <- heap.size - 1;
        if S.is_empty seq then
            heap.map <- M.remove key heap.map;
        v

end

type t = {
    mutable available : worker BatSet.t;
    pending_consumers : (int, worker cont) Q.t;
}

let create () = {
    available = BatSet.empty;
    pending_consumers = Q.create ();
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
            let node = Q.insert pri k p.pending_consumers
            in
            on_cancel_w k (fun () -> Q.delete node))
        in
        L.dbg "Didn't have available, now pending" >>= fun () ->
        t

let rec put p w = 
    if Q.size p.pending_consumers = 0 then begin
        p.available <- BatSet.add w p.available;
        L.dbg "No consumers, adding to available, now have %d" 
            (BatSet.cardinal p.available)
    end else begin
        let k = Q.take_min p.pending_consumers in
        L.dbg "Found live consumer, giving directly" >>= fun () ->
        Lwt.wakeup k w;
        return ()
    end
        
        

