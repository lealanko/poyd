open NcPrelude

type 'a t

val of_channel : 'a NcChannel.t -> 'a t

val create : unit -> 'a t

val put : 'a t -> 'a -> unit lwt

val take : 'a t -> 'a lwt
