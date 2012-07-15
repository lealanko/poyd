open Lwt
open FundLwt
open Fund

module L = (val FundLog.make "poyd_servant" : FundLog.S)

module Master = PoydMasterStub

let impl = PoydServantImpl.servant

module ServantStub = PoydServantStub.Make(PoydServantImpl)

let _ = PoydArgs.(Arg.parse full_specs anon_arg usage)

let host = !PoydArgs.host
let port = !PoydArgs.port

let max_delay = 60

let hdl_ids = Queue.create ()


let handler signum =
    ignore (L.notice "Received signal %d, aborting" signum);
    Queue.iter Lwt_unix.disable_signal_handler hdl_ids;
    Queue.clear hdl_ids;
    PoydServantImpl.abort impl

let add_handler signum = 
    Queue.add (Lwt_unix.on_signal signum handler) hdl_ids

let _ = List.iter add_handler [Sys.sigterm; Sys.sigint]

let main () =
    ServantStub.create impl >>= fun stub ->
    PoydServantImpl.get_name impl >>= fun name ->
    L.info "This is poyd servant %s" name >>= fun () ->
    let rec connect_loop delay = 
        catch 
            (fun () -> connect ~host ~port ())
            (fun exn ->
                L.error "Couldn't connect to %s:%d" host port >>= fun () ->
                L.info "Sleeping for %d seconds before retrying" delay >>= fun () ->
                Lwt_unix.sleep (float_of_int delay) >>= fun () ->
                connect_loop (min (delay * 2) max_delay))
    and main_loop () =
        connect_loop 1 >>= fun conn ->
        get_root "poyd-master" >>= fun master ->
        Master.register_servant master stub >>= fun () ->
        L.info "Registered to poyd master" >>= fun () ->
        wait conn >>= fun () ->
        PoydServantImpl.disconnect impl;
        L.info "Disconnected from master, reconnecting" >>= fun () ->
        main_loop ()
    and finish () =
        PoydServantImpl.finish impl >>= fun () ->
        L.info "Servant finished cleanly"
    in
    main_loop () <?> finish ()

let () = Lwt_main.run (main ())
