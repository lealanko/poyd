open NcPrelude

exception MgrClosed

module type S = sig
    val task : (unit -> 'a lwt) -> 'a lwt
    val close : unit -> unit lwt
    val finish : unit lwt
end

type t = (module S)

