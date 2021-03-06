open PoydPrelude

module L = (val FundLog.make "PoydServantMain" : FundLog.S)

module Master = PoydMasterStub

let impl = PoydServantImpl.servant

module ServantStub = PoydServantStub.Make(PoydServantImpl)

let max_delay = 60

let main specs args =
    let current = ref 0 
    in
    let _ = PoydArgs.(Arg.parse_argv ~current args specs anon_arg usage)
    in
    let host = !PoydArgs.host
    and port = !PoydArgs.port
    in
    ServantStub.create impl >>= fun stub ->
    PoydServantImpl.get_name impl >>= fun name ->
    L.info "This is poyd servant %s" name >>= fun () ->
    let rec connect_loop delay = 
        catch 
            (fun () -> L.trace_ (fun () -> connect ~host ~port ())
                "connect")
            (function 
              | Unix.Unix_error (Unix.ECONNREFUSED, _, _) -> begin
                  (if delay = 1
                   then L.info "Couldn't connect to %s:%d, retrying..." host port
                   else return ()) >>= fun () ->
                  L.dbg "Sleeping for %d seconds before retrying" delay 
                  >>= fun () ->
                  Lwt_unix.sleep (float_of_int delay) >>= fun () ->
                  connect_loop (min (delay * 2) max_delay)
              end
              | exn -> fail exn)
    and main_loop () =
        try_bind (fun () -> connect_loop 1)
            (fun conn ->
                L.notice "Connected to %s:%d" host port >>= fun () ->
                get_root "poyd-master" >>= fun master ->
                L.trace_ (fun () -> Master.register_servant master stub)
                    "Register to poyd master" >>= fun () ->
                wait conn >>= fun () ->
                PoydServantImpl.disconnect impl;
                L.notice "Disconnected from master, reconnecting" >>= fun () ->
                main_loop ())
            (function
              | Not_found ->
                  L.error "Unknown address: %s" host
              | exn -> fail exn)
    and finish () =
        PoydServantImpl.finish impl >>= fun () ->
        L.info "Servant finished cleanly"
    in
    main_loop () <?> finish ()
