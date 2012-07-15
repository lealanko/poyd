open PoydDefs

include SERVANT with module Client = PoydClientStub

val servant : t

val abort : t -> unit

val disconnect : t -> unit

val finish : t -> unit lwt
