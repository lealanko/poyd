open PoydDefs

include SERVANT with module Client = PoydClientStub

module Make (Servant : SERVANT with module Client = PoydClientStub) : sig
    val create : Servant.t -> t lwt
end

