open Lwt
open FundLwt
open Fund

module Master = PoydMasterStub

module ClientStub = PoydClientStub.Make(PoydClientImpl)

let impl = PoydClientImpl.client

let stub = ClientStub.make impl

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



let main () =
    connect ~host ~port () >>= fun conn ->
    get_root "poyd-master" >>= fun master ->
    let run command = PoydThread.callback thr (fun () ->
        Master.run_task master stub command) in
    PoydThread.run thr (fun () ->
        MainUtil.main script run !Arguments.just_exit) >>= fun r ->
    disconnect conn >>= fun () ->
    Lwt_io.printl "done!" >>= fun () ->
    return r

let retcode  = Lwt_main.run (main ())
let _ = exit retcode
