module type S = sig
    val open_connections : Connection.t set signal
    val close : unit -> unit
    val set_root : string -> 'a -> unit
end

type t = (module S)
