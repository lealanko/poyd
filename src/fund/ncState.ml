open NcPrelude

type 'a t = {
    mutable state : 'a;
    waiters : (('a -> bool) * 'a cont) seq;
    eq_waiters : ('a, unit cont seq) Hashtbl.t;
}

let create state = {
    state;
    waiters = Seq.create ();
    eq_waiters = Hashtbl.create 5;
        
}

let wait s check = 
    if check (s.state) then
        return s.state
    else
        ccc (fun k -> Seq.push_cont_value s.waiters k (check, k))

let wait_eq s state =
    if s.state = state then
        return ()
    else begin
        if not (Hashtbl.mem s.eq_waiters state) then
            Hashtbl.add s.eq_waiters state (Seq.create ());
        let seq = Hashtbl.find s.eq_waiters state in
        ccc (Seq.push_cont seq)
    end

let set s state =
    s.state <- state;
    begin
        match Hashtbl.find_option s.eq_waiters state with
        | Some seq ->
            Seq.iter_node_l (fun node ->
                Seq.remove node;
                wakeup (Seq.get node) ()) seq
        | None -> ()
    end;
    let tmp = Seq.create () in
    (* First remove all, then wakeup, to ensure re-entrancy. *)
    Seq.iter_node_l (fun node ->
        let (check, k) = Seq.get node in
        if check state then begin
            Seq.remove node;
            ignore (Seq.add_r k tmp)
        end) s.waiters;
    Seq.iter_l (fun k -> wakeup k state) tmp
