open NcPrelude

type state = 
    | Running
    | Closing
    | Closed

type t = {
    mutable state : state;
    cancels : (unit -> unit) seq;
    close_task : unit task;
}



let create () = {
    state = Running;
    cancels = Seq.create ();
    close_task = task ();
}

exception Not_running

let check_close m =
    if m.state = Closing && Seq.is_empty m.cancels then begin
        m.state <- Closed;
        wakeup (snd m.close_task) ()
    end

let task m thunk = 
    if m.state <> Running then
        fail Not_running
    else 
        let t = apply thunk () in
        let node = Seq.push m.cancels (fun () -> cancel t) in
        finalize (const t)
            (fun () -> Seq.remove node; check_close m; return ())

let close m = 
    if m.state = Running then
        m.state <- Closing;
    fst (m.close_task)

let abort m =
    if m.state = Running then
        m.state <- Closing;
    Seq.iter_l (fun f -> f ()) m.cancels
