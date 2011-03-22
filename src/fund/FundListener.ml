open FundPrelude
open FundDefs

let listen listen_addr router = (module struct
    module R = (val router : ROUTER)
    let local_port = (module R : PORT)

    let serve (in_ch, out_ch) =
        detach (fun () ->
            FundConnection.make in_ch out_ch local_port >>= fun c ->
            let module C = (val c : CONNECTION) in
            let port = (module C : PORT) in
            R.link port >>= fun link ->
            C.finish >>= fun () ->
            R.unlink link)
        
    let server = Lwt_io.establish_server listen_addr serve

    let close () =
        Lwt_io.shutdown_server server;
        return ()
end : LISTENER)

