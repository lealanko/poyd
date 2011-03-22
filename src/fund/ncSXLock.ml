
open NcPrelude

type state = 
    | Aborted
    | Free
    | XLock
    | SLock of int

type mode =
    | Shared
    | Exclusive

type t = {
    mutable state : state;
    exclusive : unit cont seq;
    shared : unit cont seq;
    cancels : (unit -> unit) seq;
}

let create () = {
    state = Free;
    exclusive = Seq.create ();
    shared = Seq.create ();
    cancels = Seq.create ();
}

let push (seq : unit cont seq) (k : unit cont) =
    let node = Seq.add_r k seq in
    on_cancel_w k (fun () -> Seq.remove node)

let take_all seq =
    let l = ref [] in
    Seq.iter_node_r (fun node ->
	l := Seq.get node :: !l;
	Seq.remove node) seq;
    !l

exception Abort

let sync l = match l.state with
    | Aborted ->
	let cs = take_all l.cancels in
	List.iter (fun c -> c ()) cs;
	let ks = take_all l.shared @ take_all l.exclusive in
	List.iter (fun k -> wakeup_exn k Abort) ks
    | Free when not (Seq.is_empty l.exclusive) ->
	l.count <- -1;
	wakeup (Seq.take_l l.exclusive) ()
    | Free | SLock ->
	let ks = take_all l.shared in
	l.count <- l.count + List.length ks;
	List.iter (fun k -> wakeup k ()) ks

let lock l mode = 
    let q = match mode with Shared -> l.shared | Exclusive -> l.exclusive in
    ccc (fun k -> push q k; sync l)

let unlock k l =
    l.state <- match l.state with
    | XLock | SLock 0 -> Free
    | SLock n -> SLock (n - 1)
    | Free -> assert false;
    sync l

let locking l mode thunk =
    lock l mode >>= fun () ->
    fix (fun t ->
	let node = Seq.add_r t l.cancels (fun () -> cancel t) in
	finalize thunk
	    (fun () -> Seq.remove node; unlock l; return ()))

let abort l =
    l.mode <- Aborted;
    sync l
	
