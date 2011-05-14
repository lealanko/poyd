
val register : exn -> unit

val immigrate : ?strict:bool -> exn -> exn
(** if [strict = true], raise Invalid_argument if the argument exception
    class has not been registered. *)
