open FundPrelude

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

let server port =
    let f x = 
        dbg "f(%d)" x >>= fun () ->
        return (x * x) in
    Fund.listen ~local:true ~port () >>= fun () ->
    let h = Fund.publish f in
    Fund.set_root "f" h >>= fun () ->
    halt ()

let client host port =
    dbg "begin connect" >>= fun _ ->
    Fund.connect ~host ~port () >>= fun () ->
    dbg "begin get_root" >>= fun _ ->
    Fund.get_root "f" >>= fun f ->
    dbg "got root" >>= fun _ ->
    Fund.($) f 7 >>= fun r ->
    dbg "got result" >>= fun _ ->
    IO.printl (string_of_int r)
    

let main_ () =
    let opts = parse_argv Sys.argv in
    if opts.server then
        server opts.port
    else
        client opts.host opts.port

let main () =
    Lwt_main.run (main_ ())

let _ = main()
