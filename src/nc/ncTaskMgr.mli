
open NcPrelude

type t

exception Not_running

val create : unit -> t

val task : t -> (unit -> 'a lwt) -> 'a lwt

val close : t -> unit lwt

val abort : t -> unit

