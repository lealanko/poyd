open PoydDefs

module type S = sig
    type t
    val set_client : t -> PoydClient.t -> unit lwt
    val execute_script : t -> script list -> unit lwt
    val set_trees : t -> tree Sexp.t -> unit lwt
    val get_trees : t -> tree Sexp.t rmh
    val set_data : t -> Data.d -> unit lwt
    val get_data : t -> Data.d lwt
    val set_rng : t -> Rng.t -> unit lwt
end
