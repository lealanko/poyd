open NcPrelude

type ('a, 'r) handle

val (!!) : ('a, 'r) handle -> 'a -> 'r lwt

val publish : ('a -> 'r lwt) -> ('a, 'r) handle
val publish2: ('a -> 'b -> 'r lwt) -> (('a * 'b), 'r) handle
val publish3 : ('a -> 'b -> 'c -> 'r lwt) -> (('a * 'b * 'c), 'r) handle

val connect : Unix.sockaddr -> unit lwt
(* val disconnect : Unix.sockaddr -> unit lwt *)

val get_root : string -> 'a lwt

val set_root : string -> 'a -> unit lwt

val listen : Unix.sockaddr -> unit lwt

(* val finish : unit -> unit lwt *)


