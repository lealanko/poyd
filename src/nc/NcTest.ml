
open NcPrelude

let section = Lwt_log.Section.make "NcTest"

type opts = {
    server : bool;
    port : int;
    host : string;
}

let dbg fmt = Lwt_log.debug_f ~section fmt

let parse_argv argv =
    let open BatOptParse in
    let p = OptParser.make ()
    and serverp = StdOpt.store_true () 
    and port = StdOpt.int_option () 
    and host = StdOpt.str_option ~default:"localhost" ()
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
        dbg "f(%d)" x >>= fun () ->
        return (x * x) in
    get_addr host port >>= fun addr ->
    API.listen addr >>= fun () ->
    let h = API.publish f in
    API.set_root "f" h >>= fun () ->
    halt ()

let client host port =
    dbg "begin client" >>= fun _ ->
    get_addr host port >>= fun addr ->
    dbg "begin connect" >>= fun _ ->
    API.connect addr >>= fun () ->
    dbg "begin get_root" >>= fun _ ->
    API.get_root "f" >>= fun f ->
    dbg "got root" >>= fun _ ->
    API.(!!) f 7 >>= fun r ->
    dbg "got result" >>= fun _ ->
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
