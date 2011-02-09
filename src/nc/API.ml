open NcPrelude
open NcDefs

module type HANDLE = sig
    type d
    type a
    type r
    val id : d id
    val handle : (d, a, r) local_handle
end

type ('a, 'r) handle = (module HANDLE with type a = 'a and type r = 'r)

let mk_handle (type d_) (type a_) (type r_) id lh = (module struct 
    type d = d_
    type a = a_
    type r = r_
    let id = id
    let handle = lh
end : HANDLE with type a = a_ and type r = r_)

let local = Domain.local ()

module D = (val local : LOCAL_DOMAIN)

let local_port = Domain.port (module D : DOMAIN)

module R = LocalRouter

let local_link = R.link local_port

let (!!) (type a_) (type r_) h =
    let module H = (val h : HANDLE with type a = a_ and type r = r_) in
    R.apply H.id H.handle
    

let router = (module R : ROUTER)

let router_port = (module R : PORT)

let publish f =
    mk_handle D.id (D.register_handle f)

let publish2 f =
    publish (fun (a, b) -> f a b)

let publish3 f =
    publish (fun (a, b, c) -> f a b c)

let connections = Lwt_sequence.create ()

let connect addr =
    Lwt_io.open_connection addr >>= fun (in_ch, out_ch) ->
    Connection.make in_ch out_ch router_port >>= fun conn ->
    let node = Lwt_sequence.add_l conn connections in
    let module C = (val conn : CONNECTION) in
    let port = (module C : PORT) in
    detach (fun () ->
        R.link port >>= fun link ->
        finalize (fun () -> C.finish)
            (fun () ->
                Lwt_sequence.remove node;
                R.unlink link));
    return () 
        
let get_root str = 
    let id = PolyMap.UuidKey.unsafe_of_string str in
    R.get_root id

let set_root str a = 
    let id = PolyMap.UuidKey.unsafe_of_string str in
    D.set_root id a;
    return ()

let listeners = Lwt_sequence.create ()

let listen addr = 
    let listener = Listener.listen addr router in
    let _node = Lwt_sequence.add_l listener listeners in
    return ()

let _ = Lwt_main.at_exit (fun () ->
    Seq.iter_s (fun conn ->
        let module C = (val conn : CONNECTION) in
        C.close ()) connections)
    
