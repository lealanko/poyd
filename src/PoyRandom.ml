
module S = Random.State

type t = S.t

let state = ref (S.make [| 27182818 |])

let bits () = S.bits !state
let int bound = S.int !state bound
let float scale = S.float !state scale
let bool () = S.bool !state
let full_init seed = 
    state := S.make seed
let init seed = full_init [| seed |]

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

let fork () = S.make (Array.init 55 (fun _ -> S.bits !state))

let with_state s thunk =
  let old_state = !state
  in
  state := S.copy s;
  finally thunk (fun () -> state := old_state)

let forked thunk = with_state (fork ()) thunk

type seed_data = {
  time : float;
  pid : int;
  ppid : int;
  hostname : string;
}

let mk_seed_data () =
  let open Unix in {
    time = begin
      try gettimeofday ()
      with Invalid_argument _ -> time ()
    end;                                
    pid = getpid ();
    ppid = getppid ();
    hostname = gethostname ();
  }

open Unix

let sys_state = 
    let time =
        try gettimeofday ()
        with Invalid_argument _ -> time () in
    let hostname = gethostname () in
    let fqdn = try
                   let host = gethostbyname hostname in
                   host.h_name
        with Not_found -> hostname in
    let data = (time, fqdn, Unix.getpid()) in
    let buf = Marshal.to_string data [] in
    let arr = Array.make (String.length buf) 0 in
    for i = 0 to String.length buf - 1 do
        Array.set arr i (int_of_char (String.get buf i))
    done;
    S.make arr
