let port = ref 7693

(*
let set_verbose () =
    Lwt_log.add_rule "Poyd*" Lwt_log.Info;
    Lwt_log.add_rule "poyd*" Lwt_log.Info
*)

let common_specs = [
    ("-p", Arg.Set_int port, 
     Printf.sprintf "port of master (default %d)" !port);
(*
    ("-v", Arg.Unit set_verbose, 
     "verbose output") *)
]

let host = ref "localhost"

let usage = Printf.sprintf
    "Usage: %s [OPTIONS]" Sys.argv.(0)

let full_specs = [
    ("-h", Arg.Set_string host,
     Printf.sprintf "hostname of master (default %s)" !host)
] @ common_specs

let anon_arg s =
    raise (Arg.Bad (Printf.sprintf "Invalid argument: %s" s))



