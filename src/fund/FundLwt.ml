open Lwt

type 'a lwt = 'a Lwt.t

type 'a cont = 'a Lwt.u

type 'a task = 'a lwt * 'a cont

let halt () : 'a lwt =
    let t, u = wait () in
    t

let graft (k : 'a cont) (v : 'a) : 'b lwt =
    wakeup k v;
    halt ()

let on_cancel_w (k : 'a cont) (f : unit -> unit) : unit =
    Lwt.on_cancel (waiter_of_wakener k) f

let tie_nocancel (t : 'a lwt) (k : 'a cont) : unit =
    ignore (try_bind (fun () -> t)
		(fun a -> wakeup k a; return ())
		(fun exn -> wakeup_exn k exn; return ()))

let tie (t : 'a lwt) (k : 'a cont) : unit =
    tie_nocancel t k;
    on_cancel_w k (fun () -> cancel t)

let ( >>- ) = tie

let on_exn thunk hdl =
    catch thunk (fun exn -> hdl exn >>= fun () -> fail exn)

let cont (f : 'a -> unit) : 'a cont =
    let t, u = task () in
    ignore (t >>= fun x -> f x; return ());
    u

let ccc (f : 'a cont -> unit) : 'a lwt =
    let t, u = task () in
    f u; 
    t

let fix (f : 'a lwt -> 'a lwt) : 'a lwt =
    ccc (fun k ->
	f (waiter_of_wakener k) >>- k)

let choose_split l =
    nchoose_split l >>= fun ((t :: succ), waiters) -> 
    return (t, List.map return succ @ waiters)

let detach thunk =
    ignore (
	catch thunk
	    (fun exn ->
		Lwt_log.debug_f ~exn "detached thunk failed!" >>= fun () ->
		fail exn))

let no_cancel t =
    let t2, k2 = wait () in
    t >>- k2;
    t2

let block t =
    finalize
        (fun () -> protected t)
        (fun () -> no_cancel t >|= ignore)

let wait_thread (t : unit lwt) : unit lwt =
    let t2 = 
        protected 
            (catch 
                 (fun () -> t)
                 (function
                   | Canceled -> return ()
                   | exn -> fail exn)) in
    on_cancel t2 (fun () -> cancel t);
    t2

open BatPervasives

let result thunk =
    try_bind thunk
        (fun ret -> return (Ok ret))
        (fun exn -> return (Bad exn))

let run_result = function
    | Ok ret -> return ret
    | Bad exn -> fail exn

let wakeup_result u = function
    | Ok ret -> wakeup u ret
    | Bad exn -> wakeup_exn u exn
