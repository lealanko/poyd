
open NcPrelude

type 'a t

val create : unit -> 'a t

val put : 'a t -> 'a -> unit

val take : 'a t -> 'a lwt
