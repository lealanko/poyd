open Lwt
module Seq = Lwt_sequence
type 'a lwt = 'a Lwt.t
type 'a cont = 'a Lwt.u
type 'a seq = 'a Seq.t


type 'a t = {
    mutable readers : 'a cont seq;
    mutable writers : ('a * unit cont) seq;
}

let create () = { 
    readers = Seq.create ();
    writers = Seq.create ();
}

let sync_ rk v wk = wakeup rk v; wakeup wk ()

let put_ c v wk = match Seq.take_opt_l c.readers with
    | None -> let node = Seq.add_r (v, wk) c.writers in
	      on_cancel wk (fun () -> Seq.remove node)
    | Some rk -> sync rk v wk

let put c v = ccc (put_ c v)

let take_ c rk = match Seq.take_opt_l c.writers with
    | None -> let node = Seq.add_r rk c.readers in
	      on_cancel rk (fun () -> Seq.remove node)
    | Some (v, wk) -> sync_ rk v wk

let take c = ccc (take_ c)




let try_put c v = match Seq.take_opt_l c.readers with
    | Some k -> wakeup k v; true
    | None -> false







let put c v = 
    if try_put c v 
    then return ()
    else call_cc <| fun k ->
	let node = Seq.add_r (v, k) c.writers in
	on_cancel k (fun () -> Seq.remove node)
	    

let try_take c = Seq.take_opt_l c.writers |> 
	Option.map (fun (v, k) -> wakeup k (); v)

let take c = match try_take c with
    | Some v -> return v
    | None -> call_cc <| fun k ->
	let node = Seq.add_r k c.readers in
	on_cancel k (fun () -> Seq.remove node)
