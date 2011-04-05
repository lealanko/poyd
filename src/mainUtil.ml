
let welcome_msg () = 
    let out = Status.user_message Status.Information in
    let rephrase str = Str.global_replace (Str.regexp " +") "@ " str in
    out "";
    out "";
    out "";
IFDEF USENCURSES THEN
    out (rephrase "@[Type commands in the middle window, titled Interactive \
    Console.@\nJob status will appear below, and output will appear here.@\nA \
    summary of POY's current state will appear to the right of the \
    console.@\nFor help, type @{<u>help()@}.@\n@\nEnjoy!@]")
ELSE
    out (rephrase "@[For help, type @{<u>help()@}.@\n@\nEnjoy!@]")
END
    

let load_script files =
    try
        let lst = List.rev files in
        Some (PoyCommand.read_script_files false lst)
    with
    (*
        | Stdpp.Exc_located (_, PoyCommand.Exit) -> exit 0
        | Stdpp.Exc_located ((a, b), Stream.Error err) 
        | Stdpp.Exc_located ((a, b), Token.Error err) ->
              let is_unknown = "illegal begin of expr" = err in
              let msg = "@[Command error between characters @{<b>" ^ 
                string_of_int a.Lexing.pos_cnum ^ "@} and @{<b>" ^
                string_of_int b.Lexing.pos_cnum ^ "@}: @["^
                (if is_unknown then "Unknown command" else
                    err) ^ "@]@]" in
                Status.user_message Status.Error msg;
                if !(Arguments.just_exit) then exit 1
                else None
                *)
        | err ->
                let msg = StatusCommon.escape (Printexc.to_string err) in
                Status.user_message Status.Error msg;
                None

let begin_sadman () =
    (* let () = SadmanOutput.establish_connection () in *)
    let () = SadmanOutput.output
        "<?xml version=\"1.0\"?><!-- -*- mode: xml; mode: auto-revert; -*- -->\n" in
    let () = SadmanOutput.output "<sadman version=\"1.0\">\n" in
    let timer = Timer.start () in

    let timestring =
        let time = Unix.time () in
        let tm = Unix.gmtime time in
        Printf.sprintf "%d-%02d-%02d %02d:%02d:%02d"
            (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
            tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec in

    let hostname = Unix.gethostname () in

    let () = Sadman.start "POY"
        ((Sadman.prefix "module-version" (SadmanOutput.get_module_versions ()))
         @ (SadmanOutput.runtime_properties)
         @ [("hostname", hostname); ("timestamp", timestring)]) in

    let safe_exit () =
        let time = Timer.get_user timer in
        let () = Sadman.finish [("exit-code", "0");
                                ("execution-time", string_of_float time)] in
        
        SadmanOutput.output "</sadman>\n";
IFDEF USEPARALLEL THEN
        Mpi.finalize ()
ELSE
        ()
END
    in
    let () = at_exit safe_exit in
    ()


let rec all_matches ?(group=0) ?(start=0) regexp string =
    let matcher =
        if group = 0
        then Str.matched_string
        else Str.matched_group group in
    try
        let m = Str.search_forward regexp string start in
        let m_string = matcher string in
        m_string :: (all_matches ~start:(m + 1) ~group regexp string)
    with _ -> []

let unknown_command_regexp = Str.regexp "^[ \t]*\\([a-zA-Z_]*\\)"

(* Useful regex for matching tokens in syntax errors *)
let token_regexp = Str.regexp "\\[\\([a-zA-Z_]*\\)\\]"

(** Catch errors or not;  helpful for debugging *)
let debug_pass_errors = false

let main script run just_exit =
    let initial_script = ref script in
    let input = ref "" in
    let proc_command str =
        let () = Sys.catch_break true in
        try
            let command = 
                let master =
IFDEF USEPARALLEL THEN
                my_rank = 0 
ELSE
                true
END
                in
                if master then
                    let comm =
                        match !initial_script with
                        | Some comm -> 
                                initial_script := None;
                                comm
                        | None -> []
                    in
                    if 0 = String.length str then comm
                    else begin
                        input := str;
                        comm @ (PoyCommand.of_string false str)
                    end
                else []
            in
            let command =
IFDEF USEPARALLEL THEN
                let command = Analyzer.analyze command in
                let command = Mpi.broadcast command 0 Mpi.comm_world in
                let size = Mpi.comm_size Mpi.comm_world in
                Analyzer.parallel_analysis my_rank size command
ELSE
                match command with
                | [_] -> command
                | x -> Analyzer.analyze command
END
            in
            run command
        with
        | Camlp4.PreCast.Loc.Exc_located (_, PoyCommand.Exit) -> exit 0
        | Camlp4.PreCast.Loc.Exc_located (a, Stream.Error err) ->
                let beg = Camlp4.PreCast.Loc.start_off a
                and en = Camlp4.PreCast.Loc.stop_off a in
              let is_unknown = "illegal begin of expr" = err in
              let msg = "@[<v 4>Command error between characters @{<b>" ^ 
                string_of_int beg ^ "@} and @{<b>" ^
                string_of_int en ^ "@}:@,@["^
                (if is_unknown then "Unknown command" else
                    err) ^ "@]@]\n" in
                Status.user_message Status.Error msg;
                Status.error_location beg en;
                let elements =
                    if is_unknown
                    then all_matches ~group:1 unknown_command_regexp !input
                    else all_matches ~group:1 token_regexp err in
                List.iter HelpIndex.help_if_exists elements;
                if just_exit || not (Status.is_interactive ()) 
                    then exit 1
                else ()
        | Sys.Break -> 
                Status.clear_status_subwindows ();
                Status.user_message Status.Error "Interrupted";
                ()
        | Methods.TimedOut ->
                Status.clear_status_subwindows ();
                let msg = "Search timed out" in
                Status.user_message Status.Information msg
        | err when ((not just_exit && (Status.is_interactive ())) && 
                (not debug_pass_errors)) ->
                Status.clear_status_subwindows ();
                let msg = StatusCommon.escape (Printexc.to_string err) in
                Status.user_message Status.Error msg
        | err when just_exit ->
                let msg = StatusCommon.escape (Printexc.to_string err) in
                Status.user_message Status.Error msg;
                raise err
    in
IFDEF USEPARALLEL THEN
    if 0 = my_rank then
        Status.main_loop proc_command
    else 
        while true do
            proc_command ""
        done
ELSE
    Status.main_loop proc_command
END
    
