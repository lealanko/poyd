open NcPrelude

module S = Lwt_sequence


type 'a t = {
    values : 'a S.t;
    readers : 'a Lwt.u S.t;
}

let create () = {
    values = S.create ();
    readers = S.create ();
}

let put q v = 
    match S.take_opt_l q.readers with
    | Some r -> Lwt.wakeup r v
    | None -> ignore (S.add_r v q.values)

let try_take q = S.take_opt_l q.values

let take q = match try_take q with
    | Some v -> Lwt.return v
    | None -> let (t, u) = Lwt.task () in
	      let node = Lwt_sequence.add_r u q.readers in
	      Lwt.on_cancel t (fun _ -> S.remove node);
	      t

