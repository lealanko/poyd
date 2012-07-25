open PoydPrelude

module L = (val FundLog.make "poyd_servant" : FundLog.S)

let hdl_ids = Queue.create ()

let handler signum =
    ignore (L.notice "Received signal %d, aborting" signum);
    Queue.iter Lwt_unix.disable_signal_handler hdl_ids;
    Queue.clear hdl_ids;
    PoydServantImpl.abort PoydServantImpl.servant

let add_handler signum = 
    Queue.add (Lwt_unix.on_signal signum handler) hdl_ids

let _ = List.iter add_handler (match Sys.os_type with
    | "Win32" -> [Sys.sigint] (* sigterm is fubar on mingw *)
    | _ -> [Sys.sigterm; Sys.sigint])

let () = Lwt_main.run (PoydServantMain.main PoydArgs.full_specs Sys.argv)
