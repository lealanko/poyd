
open NcPrelude

type t

type state = Initial | Connecting | Connected | Closing | Closed

val create : Domain.Map.t signal -> t

val connect : t -> Unix.sockaddr -> unit lwt

val remote_domains : t -> Domain.Map.t signal

val state : t -> state signal

val close : t -> unit lwt

val abort : t -> unit

type connection = t

module Listener = struct
    type t
    val create : Domain.Map.t signal -> t
    val listen : t -> sockaddr -> unit lwt
    val open_connections : t -> connection set signal
end

