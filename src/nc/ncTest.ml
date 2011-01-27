
open NcPrelude

type opts = {
    server : bool;
    port : int;
    host : string;
}

let parse_argv argv =
    let open BatOptParse in
    let p = OptParser.make ()
    and serverp = StdOpt.store_true () 
    and port = StdOpt.int_option () 
    and host = StdOpt.str_option ~default:"localhost" ()
    and help = StdOpt.help_option ()
    in
    OptParser.add p ~short_name:'s' serverp;
    OptParser.add p ~short_name:'p' port;
    OptParser.add p ~short_name:'a' host;
    ignore (OptParser.parse p argv);
    {
        server = Opt.get serverp;
        port = Opt.get port;
        host = Opt.get host;
    }



let get_addr host port = 
    Lwt_unix.gethostbyname host >>= fun entry ->
    return (Unix.ADDR_INET (Array.get entry.Unix.h_addr_list 0, port))


let server host port =
    let f x = 
        Lwt_log.debug_f "f(%d)" x >>= fun () ->
        return (x * x) in
    let module LocalDomain = Domain.Local(Unit) in
    let local_domain = (LocalDomain : Domain.DOMAIN) in
    let domains = 
        Signal.const (Domain.Map.insert Domain.Map.empty local_domain) in
    get_addr host port >>= fun addr ->
    let listener = Connection.create_listener addr domains in
    let module L = (val listener : Connection.LISTENER) in
    L.set_root "f" f;
    halt ()

let client host port =
    get_addr host port >>= fun addr ->
    Connection.connect addr >>= fun conn ->
    let d = Domain.Map.get lookup
    NcConnection.get_root conn "f" >>= fun f_h ->
    NcHandle.apply f_h 7 >>= fun r ->
    IO.printl (string_of_int r)
    

let main_ () =
    let opts = parse_argv Sys.argv in
    if opts.server then
        server opts.host opts.port
    else
        client opts.host opts.port

let main () =
    Lwt_main.run (main_ ())

let _ = main()
