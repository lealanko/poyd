
val register : exn -> unit

val immigrate : exn -> exn
(** @raise Invalid_argument if the argument exception class has not been 
    registered. *)
