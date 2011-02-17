open NcPrelude

type 'a t
val create : unit -> 'a t

type 'a output
val output : 'a t -> 'a output
val put : 'a output -> 'a -> unit lwt

type 'a input
val input : 'a t -> 'a input
val take : 'a input -> 'a lwt
