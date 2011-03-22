open NcLwt

type t

type mode = Shared | Exclusive

val create : unit -> t

val locking : t -> mode -> (unit -> 'a lwt) -> 'a lwt

val abort : t -> unit
