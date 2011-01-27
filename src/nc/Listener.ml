module type S = sig

    val close : unit -> unit lwt

end




module type LISTENER_ARGS = sig
    val local_domains : Domain.Map.m signal
    val listen_addr : Unix.sockaddr
end

module Listener(LArgs : LISTENER_ARGS) : LISTENER = struct
    let change_event, send_change = Event.create ()

    let roots, set_roots = Signal.create (Map.empty)

    let open_connections = 
        Signal.fold (fun old c -> c old) Set.empty change_event

    let serve (in_ch, out_ch) =
        let module CArgs = struct
            include LArgs
            let in_ch = in_ch
            let out_ch = out_ch
            let roots = roots
        end in
        let module C = Connection(CArgs) in
        let c = (module C : CONNECTION) in
        send_change (Set.add c);
        detach (fun () ->
            finalize (fun () -> C.finish)
                (fun () ->
                    return (send_change (Set.remove c))))

    let server = Lwt_io.establish_server LArgs.listen_addr serve

    let set_root k v =
        set_roots (Map.add k (Obj.repr v) (Signal.value roots))

    let close () = 
        Lwt_io.shutdown_server server
end

let listen listen_addr local_domains =
    let module LArgs = struct
        let local_domains = local_domains
        let listen_addr = listen_addr
    end in
    let module L = Listener(LArgs) in
    (module L : LISTENER)

