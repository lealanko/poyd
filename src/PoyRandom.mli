
type t = Random.State.t

val fork : t -> t

val with_state : t -> (unit -> 'a) -> 'a

val forked : (unit -> 'a) -> 'a

val int : int -> int
val float : float -> float
val bool : unit -> bool
val init : int -> unit
val full_init : int array -> unit
val bits : unit -> int
