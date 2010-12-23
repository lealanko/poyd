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

let tie (t : 'a lwt) (k : 'a cont) : unit =
    ignore (try_bind (fun () -> t)
		(fun a -> wakeup k a; return ())
		(fun exn -> wakeup_exn k exn; return ()));
    on_cancel_w k (fun () -> cancel t)

let ( >>- ) = tie


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

let result (thunk : unit -> 'a lwt) =
    catch
	(fun () -> thunk () >|= fun v -> BatStd.Ok v)
	(fun exn -> return (BatStd.Bad exn))

let detach thunk =
    ignore (
	catch thunk
	    (fun exn ->
		Lwt_log.debug_f ~exn "detached thunk failed!" >>= fun () ->
		fail exn))

