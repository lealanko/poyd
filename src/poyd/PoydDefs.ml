include PoydPrelude

include ScriptingTypes
include ScriptingTypes.Defs(Phylo.Types)

module type CLIENT = sig
    type t
    val request_file : t -> string -> string lwt
    val explode_filenames : t -> Parser.filename list -> string list lwt
    val output_status : t -> Status.c -> string -> unit lwt
end

type taxon_codes = string All_sets.IntegerMap.t

module type SERVANT = sig
    module Client : CLIENT
    type t
    val set_client : t -> Client.t -> unit lwt
    val execute_script : t -> script list -> unit lwt
    val set_trees : t -> tree Sexpr.t -> unit lwt
    val set_stored_trees : t -> tree Sexpr.t -> unit lwt
    val get_trees : t -> tree Sexpr.t lwt
    val get_stored_trees : t -> tree Sexpr.t lwt
    val set_data : t -> Data.d -> unit lwt
    val get_data : t -> Data.d lwt
    val set_rng : t -> Rng.t -> unit lwt
    val get_rng : t -> Rng.t lwt
    val set_run : t -> r -> unit lwt
    val get_run : t -> r lwt
(*
    val get_jackknife : t -> (support_class * taxon_codes) lwt
    val add_jackknife : t -> (support_class * taxon_codes) -> unit lwt
*)
        
end

module type MASTER = sig
    module Client : CLIENT
    module Servant : SERVANT with module Client = Client
    type t
    val register_servant : t -> Servant.t -> unit lwt
    val run_task : t -> Client.t -> script list -> unit lwt
end
