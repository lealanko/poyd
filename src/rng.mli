val fork : unit -> Random.State.t

val with_state : Random.State.t -> (unit -> 'a) -> 'a

val forked : (unit -> 'a) -> 'a
