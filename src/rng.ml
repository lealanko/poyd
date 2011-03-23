
type t = Random.State.t

type 'a result = Succ of 'a | Fail of exn

let finally thunk end_thunk =
  let result = 
    try
      Succ (thunk ())
    with exn ->
      Fail exn
  in
  end_thunk ();
  match result with
    | Succ res -> res
    | Fail exn -> raise exn

let fork s = Random.State.make (Array.init 55 (fun _ -> Random.State.bits s))

let with_state s thunk =
  let old_state = Random.get_state ()
  in
  Random.set_state s;
  finally thunk (fun () -> Random.set_state old_state)

let forked thunk = with_state (fork (Random.get_state ())) thunk
