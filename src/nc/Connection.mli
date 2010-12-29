open NcPrelude

include ConnectionDefs

val connect : Unix.sockaddr -> Domain.Map.t signal -> t lwt

val listen : Unix.sockaddr -> Domain.Map.t signal -> listener lwt
