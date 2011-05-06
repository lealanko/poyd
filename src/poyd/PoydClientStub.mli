open PoydDefs

include CLIENT

module Make(Client : CLIENT) : sig
    val make : Client.t -> t
end
