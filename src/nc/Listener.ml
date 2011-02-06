open NcPrelude
open NcDefs

let listen listen_addr local_port = (module struct

    let connections = Lwt_sequence.create ()

    let serve (in_ch, out_ch) =
        detach (fun () ->
            let c = Connection.make in_ch out_ch local_port in
            let node = Lwt_sequence.add_l c connections in
            let module C = (val c : CONNECTION) in
            C.finish >>= fun () ->
            Lwt_sequence.remove node;
            return ())
        
    let server = Lwt_io.establish_server listen_addr serve

    let close () =
        (* There is almost a race condition here since we cannot wait
           until the socket has been closed. *)
        Lwt_io.shutdown_server server;
        Lwt_sequence.fold_r (fun c t ->
            let module C = (val c : CONNECTION) in
            C.close () <&> t)
            connections (return ())
            
    let abort () =
        Lwt_io.shutdown_server server;
        Lwt_sequence.fold_r (fun c t ->
            let module C = (val c : CONNECTION) in
            C.abort () <&> t)
            connections (return ())
end : LISTENER)

