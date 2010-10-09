
type t = Random.State.t

val fork : unit -> t

val with_state : t -> (unit -> 'a) -> 'a

val forked : (unit -> 'a) -> 'a
