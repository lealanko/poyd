open PoydPrelude

type worker = PoydServantStub.t

type t

val create : unit -> t

val get : t -> int -> worker lwt

val put : t -> worker -> unit lwt
