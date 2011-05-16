open PoydDefs

module type S = sig
    type t
    val request_file : t -> string -> string lwt
    val output_status : t -> Status.c -> string -> unit lwt
end
