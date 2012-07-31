
val set_idle_priority : unit -> unit

val run :
    name: string ->
    main: (string array -> unit Lwt.t) ->
    ?stop: (bool -> unit Lwt.t) ->
    ?pause : (bool -> unit Lwt.t) ->
    unit ->
    unit Lwt.t
    


