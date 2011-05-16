open Lwt
open FundLwt
open Fund

let () = Status.init ()

let () =
    try
        Arg.parse_argv Phylo.args Arguments.parse_list Arguments.anon_fun 
        Arguments.usage 
    with
    | Arg.Help _ ->
            Arg.usage Arguments.parse_list Arguments.usage;
            exit 0
    | Arg.Bad x ->
            prerr_string ("Bad argument: " ^ x);
            Arg.usage Arguments.parse_list Arguments.usage;
            exit 1

let () = MainUtil.welcome_msg ()


(* First load the input *)
let script = match MainUtil.load_script !Arguments.input with
    | None when !Arguments.just_exit -> exit 1
    | script -> script

let () = MainUtil.begin_sadman ()

let seed = truncate (Unix.time ())


let run command = 
    let res = 
        Phylo.run 
            ~output_file:(!(Arguments.dump_file)) 
            ~start:(Phylo.get_console_run ()) command 
    in
    Phylo.set_console_run res


let port = 7654
let host = "localhost"

let thr = PoydPoy.thread


module Master = PoydMasterStub

module C = PoydClientImpl
module CS = PoydClientStub.Make(C)

module L = (val FundLog.make ~logger:PoydPoy.status_logger "poyd_client" 
        : FundLog.S)

let main () =
    C.get_name C.client >>= fun name ->
    L.info "This is poyd client %s." name >>= fun () ->
    CS.create C.client >>= fun stub ->
    connect ~host ~port () >>= fun conn ->
    L.info "Connected to poyd master at %s:%d." host port >>= fun () ->
    get_root "poyd-master" >>= fun master ->
    let run command = PoydThread.callback thr (fun () ->
        Master.run_task master stub command) in
    PoydThread.run thr (fun () ->
        MainUtil.main script run !Arguments.just_exit) >>= fun r ->
    disconnect conn >>= fun () ->
    L.info "Disconnected from poyd master." >>= fun () ->
    return r

let retcode  = Lwt_main.run (main ())
let _ = exit retcode
