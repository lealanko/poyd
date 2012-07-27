open PoydPrelude

module L = (val FundLog.make "poyd_servant" : FundLog.S)

let rest = Queue.create ()


let svc_main args =
    let all_args = BatArray.of_enum (
        BatEnum.append (BatArray.enum args) (BatQueue.enum rest))
    in
    L.notice "Starting service: %s" 
        (String.concat " " (Array.to_list all_args)) >>= fun () -> 
    PoydServantMain.main PoydArgs.full_specs all_args

let stop_handler is_shutdown =
    ignore (L.notice "Stopping service%s" 
                (if is_shutdown then " on shutdown" else ""));
    PoydServantImpl.abort PoydServantImpl.servant;
    return ()

let svc_name = ref "poyd"

let exe_specs = [
    ("-n", Arg.Set_string svc_name,
     Printf.sprintf
         "Set name of the service (default \"%s\")" !svc_name);
    ("--", Arg.Rest (fun arg -> Queue.add arg rest),
     Printf.sprintf
         "Begin list of service arguments.")
]


let _ = Arg.parse_argv Sys.argv exe_specs PoydArgs.anon_arg PoydArgs.usage

let _ = Lwt_log.default := WinEvent.logger !svc_name

let _ = begin
    Lwt_log.add_rule "Poyd*" Lwt_log.Info;
    Lwt_log.add_rule "poyd*" Lwt_log.Info;
end
    

let _ =
    Lwt_main.run (
        L.notice "Starting poyd servant. Service name: \"%s\"" !svc_name 
        >>= fun () ->
        L.notice "Step forward" >>= fun () ->
        catch (fun () ->
            L.notice "Now in catch, calling WinSvc.run" >>= fun () ->
            WinSvc.run 
                ~name:!svc_name ~main:svc_main
                ~stop:stop_handler ())
            (function
              | Failure msg -> L.error "%s" msg
              | _ -> return ()) >>= fun () ->
        L.notice "WinSvc done")
                
    

