open FundPrelude
open FundDefs

exception ConnectionError = FundConnection.ConnectionError
exception RouteError = FundRouter.RouteError

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

let local = FundDomain.local ()

module D = (val local : LOCAL_DOMAIN)

let local_port = FundDomain.port (module D : DOMAIN)

module R = FundLocalRouter

let local_link = R.link local_port

let ($) (type a_) (type r_) h =
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

let withdraw (type a_) (type r_) h = 
    let module H = (val h : HANDLE with type a = a_ and type r = r_) in
    let module T2 = struct
        type ('d, 'b_) t2 = ('d, a_, r_) local_handle
    end in
    let module Cast = FundPolyMap.UuidKey.Cast2(T2) in
    match Cast.typematch H.id D.id with
    | None -> raise Not_found
    | Some eq ->
        let handle = FundType.cast eq H.handle in
        D.unregister_handle handle

let with_handle f body =
    let h = publish f in
    finalize (fun () -> body h) (fun () -> withdraw h; return ())

type connection = FundDefs.connection

let connections = Lwt_sequence.create ()


let parse_addr = function
    | None, Some h, Some p, None -> begin
        Lwt_unix.gethostbyname h >>= fun entry ->
        return (Unix.ADDR_INET (Array.get entry.Unix.h_addr_list 0, p))
    end
    | Some a, None, None, None ->
        return a
    | None, None, None, Some p ->
        return (Unix.ADDR_UNIX(p))
    | _ ->
        fail (Failure "illegal arguments") 
    

let connect ?addr ?host ?port ?path () =
    parse_addr (addr, host, port, path) >>= fun sockaddr ->
    Lwt_io.open_connection sockaddr >>= fun (in_ch, out_ch) ->
    FundConnection.make in_ch out_ch router_port >>= fun conn ->
    let node = Lwt_sequence.add_l conn connections in
    let module C = (val conn : CONNECTION) in
    let port = (module C : PORT) in
    detach (fun () ->
        R.link port >>= fun link ->
        finalize (fun () -> C.finish)
            (fun () ->
                Lwt_sequence.remove node;
                R.unlink link));
    return conn

let disconnect conn =
    let module C = (val conn : CONNECTION) in
    C.close ()

let wait conn =
    let module C = (val conn : CONNECTION) in
    C.finish
        
let get_root str = 
    let id = FundPolyMap.UuidKey.unsafe_of_string str in
    R.get_root id

let set_root str a = 
    let id = FundPolyMap.UuidKey.unsafe_of_string str in
    D.set_root id a;
    return ()

let listeners = Lwt_sequence.create ()

let parse_listen_addr = function
    | Some a, None, None, None -> 
        return a
    | None, local, Some port, None 
        when port >= 0 && port < 65536 -> 
            let inet_addr = match local with
                | Some true -> Unix.inet_addr_loopback
                | _ -> Unix.inet_addr_any in
            return (Unix.ADDR_INET(inet_addr, port))
    | None, None, None, Some path ->
        return (Unix.ADDR_UNIX(path))
    | _ ->
        fail (Failure "illegal arguments")

let listen ?addr ?local ?port ?path () = 
    parse_listen_addr (addr, local, port, path) >>= fun addr ->
    let listener = FundListener.listen addr router in
    let _node = Lwt_sequence.add_l listener listeners in
    return ()

let _ = Lwt_main.at_exit (fun () ->
    Seq.iter_s (fun conn ->
        let module C = (val conn : CONNECTION) in
        C.close ()) connections)
    
