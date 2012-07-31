open PoydPrelude

module U = Lwt_unix


let get_fqdn () =
    U.gethostname () >>= fun hostname ->
    U.getaddrinfo hostname "" [Unix.AI_CANONNAME] >>= fun ais ->
    let ret = 
        try
            List.find (fun s -> s <> "") 
                (List.map (fun ai -> ai.Unix.ai_canonname) ais)
        with Not_found -> hostname
    in
    return ret

let get_procid () =
    get_fqdn () >>= fun fqdn ->
    let pid = Unix.getpid () in
    return (Printf.sprintf "%s/%d" fqdn pid)

let script_txt cmd = 
    Format.sprintf (StatusCommon.string_to_format 
                        (Analyzer.script_to_string cmd))

    
