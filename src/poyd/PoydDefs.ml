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

type support_type = [
    | `Jackknife
    | `Bootstrap
]

exception Abort
exception ClientExn of exn

let _ = FundExnMapper.register Abort
let _ = FundExnMapper.register (ClientExn Abort)

module type SERVANT = sig
    module Client : CLIENT
    type t
    val get_name : t -> string lwt
    val set_client : t -> Client.t option -> unit lwt
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
    val reroot : t -> unit lwt
    val begin_oneachtree : t -> script list -> script list ->
        ((PoyRandom.t -> tree -> unit lwt) * (unit -> unit lwt)) lwt
    val begin_support : t -> Methods.support_method ->
        ((PoyRandom.t -> unit lwt) * (unit -> unit lwt)) lwt
    val get_support : t -> support_type -> support_class lwt
    val begin_bremer : t -> r -> Methods.bremer_support ->
        ((PoyRandom.t -> tree -> Methods.support_tree lwt) * (unit -> unit lwt)) lwt
end

module type MASTER = sig
    module Client : CLIENT
    module Servant : SERVANT with module Client = Client
    type t
    val register_servant : t -> Servant.t -> unit lwt
    val create_task : t -> Client.t -> (script list, unit) handle lwt
end

type par_method = [
    | Methods.support_method
    | Methods.bremer_support
    | `OnEachTree of (script list * script list)
    | `ParallelPipeline of (int * script list * script list * script list)
]
