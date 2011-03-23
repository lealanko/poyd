
type t = Random.State.t

val fork : t -> t

val with_state : t -> (unit -> 'a) -> 'a

val forked : (unit -> 'a) -> 'a
