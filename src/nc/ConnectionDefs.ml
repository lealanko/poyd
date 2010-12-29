open NcPrelude

module type CONNECTION = sig
    val remote_domains : Domain.Map.t signal
    val close : unit -> unit lwt
    val abort : unit -> unit
end

type t = (module CONNECTION)

module type LISTENER = sig
    val open_connections : t set signal
    val close : unit -> unit
end

type listener = (module LISTENER)
