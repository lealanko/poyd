open NcPrelude

type 'a t

val create : 'a -> 'a t

val wait : 'a t -> ('a -> bool) -> 'a lwt

val wait_eq : 'a t -> 'a -> unit lwt

val set : 'a t -> 'a -> unit
