
open NcPrelude

type t

open React

val create : unit -> t

val connect : t -> Unix.sockaddr -> unit lwt

val set_local_domains : t -> Domain.t set signal -> unit

val remote_domains : t -> Domain.t set signal

type connection = t

module Listener = struct
    type t
    val create : Domain.t set signal -> t
    val listen : t -> sockaddr -> unit lwt
    val open_connections : t -> connection set signal
end

