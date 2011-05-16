open PoydPrelude

type t

val create : unit -> t

val run : t -> (unit -> 'a) -> 'a lwt

val callback : t -> (unit -> 'a lwt) -> 'a
