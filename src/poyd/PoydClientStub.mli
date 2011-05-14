open PoydDefs

include CLIENT

module Make(Client : CLIENT) : sig
    val create : Client.t -> t lwt
end
