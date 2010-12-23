open NcPrelude

type t

type domain

val local_domain : domain

val domain : t -> domain option

val connect : Unix.sockaddr -> t lwt

val set_root : string -> 'a -> unit

val get_root : t -> string -> 'a lwt

val lookup : domain -> t

val apply : t -> ('a, 'b) NcLocalHandle.t -> 'a -> 'b lwt


type listener

val create_listener : Unix.sockaddr -> listener

