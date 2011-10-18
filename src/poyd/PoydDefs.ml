include PoydPrelude

include ScriptingTypes
include ScriptingTypes.Defs(Phylo.Types)

type output = 
    | OutputStatus of Status.c * string
    | GetMargin of string option * int
    | SetMargin of string option * int
    | SetInformationOutput of string option 

module type CLIENT = sig
    type t
    val get_name : t -> string lwt
    val request_file : t -> string -> string lwt
    val explode_filenames : t -> Parser.filename list -> string list lwt
    val output_status : t -> Status.c -> string -> unit lwt
    val set_information_output : t -> string option -> unit lwt
    val get_margin : t -> string option -> int lwt
    val set_margin : t -> string option -> int -> unit lwt
    val wait_finish : t -> unit lwt
    val execute_output : t -> output list -> unit lwt
end

type taxon_codes = string All_sets.IntegerMap.t

module type SERVANT = sig
    module Client : CLIENT
    type t
    val get_name : t -> string lwt
    val set_client : t -> Client.t -> unit lwt
    val begin_script : t -> script list -> unit lwt
    val final_report : t -> unit lwt
    val set_trees : t -> tree Sexpr.t -> unit lwt 
    val set_stored_trees : t -> tree Sexpr.t -> unit lwt 
    val get_trees : t -> tree Sexpr.t lwt
    val get_stored_trees : t -> tree Sexpr.t lwt
    val add_trees : t -> tree Sexpr.t -> unit lwt 
    val add_stored_trees : t -> tree Sexpr.t -> unit lwt 
    val set_data : t -> Data.d -> unit lwt
    val get_data : t -> Data.d lwt
    val set_rng : t -> PoyRandom.t -> unit lwt
    val get_rng : t -> PoyRandom.t lwt
    val set_run : t -> r -> unit lwt
    val get_run : t -> r lwt
    val get_output : t -> output list lwt
    val clear_output : t -> unit lwt
    val save_original_trees : t -> unit lwt
    val clear_original_trees : t -> unit lwt
    val begin_oneachtree : t -> script list -> script list ->
        ((PoyRandom.t -> tree -> unit lwt) * (unit -> unit lwt)) lwt
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
    val create_task : t -> Client.t -> (script list, unit) handle lwt
end
