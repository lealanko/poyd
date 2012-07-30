open FundPrelude
open FundDefs

(* From Lwt_io source code, modified to expose addr *)
type server = {
  shutdown : unit Lazy.t;
}

type msg = [`Accept | `Shutdown]

let shutdown_server server = Lazy.force server.shutdown

let establish_server ?(backlog=5) sockaddr f : server =
  let open Lwt_io in
  let sock = Lwt_unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt sock Unix.SO_REUSEADDR true;
  Lwt_unix.bind sock sockaddr;
  Lwt_unix.listen sock backlog;
  let abort_waiter, abort_wakener = wait () in
  let abort_waiter = abort_waiter >>= fun _ -> return `Shutdown in
  let rec loop () =
    pick [Lwt_unix.accept sock >|= (fun x -> `Accept x); abort_waiter] >>= function
      | `Accept(fd, addr) ->
          (try Lwt_unix.set_close_on_exec fd with Invalid_argument _ -> ());
          let close = lazy begin
            Lwt_unix.shutdown fd Unix.SHUTDOWN_ALL;
            Lwt_unix.close fd
          end in
          f addr 
              (of_fd ~mode:input ~close:(fun () -> Lazy.force close) fd,
               of_fd ~mode:output ~close:(fun () -> Lazy.force close) fd);
          loop ()
      | `Shutdown ->
          Lwt_unix.close sock >>= fun () ->
          match sockaddr with
            | Unix.ADDR_UNIX path when path <> "" && path.[0] <> '\x00' ->
                Unix.unlink path;
                return ()
            | _ ->
                return ()
  in
  ignore (loop ());
  { shutdown = lazy(wakeup abort_wakener `Shutdown) }

let listen listen_addr router = (module struct
    module R = (val router : ROUTER)
    let local_port = (module R : PORT)

    let serve addr (in_ch, out_ch) =
        detach (fun () ->
            FundConnection.make ~addr in_ch out_ch local_port >>= fun c ->
            let module C = (val c : CONNECTION) in
            let port = (module C : PORT) in
            R.link port >>= fun link ->
            C.finish >>= fun () ->
            R.unlink link)
        
    let server = establish_server listen_addr serve

    let close () =
        shutdown_server server;
        return ()
end : LISTENER)

