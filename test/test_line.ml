let exit_code = ref 0
let files = ref []
let command = ref ""
let message = ref ""
let check_cost = ref []
let check_cost_less = ref []
let temp = ref "tmp.log"
let mstderr = ref None
let mstdout = ref None
let mdiff = ref None
let rmstderr = ref None
let rmstdout = ref None
let rmdiff = ref None
let costfile = ref None
let costlessfile = ref None
let error_expected = ref false

let buildbot_prefix = ref ""
let buildbot_suffix = ref ""
let buildbot = ref false

let is_first = ref true 

let for_buildbot_output flag to_strg contents =
    if not !is_first then print_string ", ";
    is_first := false;
    if !buildbot then Printf.printf "\"%s\", \"%s\" " flag (to_strg contents)
    else ()

let assign flag to_strg item v = 
    for_buildbot_output flag to_strg v;
    item := v

let assign_opt flag to_strg item v = 
    for_buildbot_output flag to_strg v;
    item := Some v


let ident x = x
let of_float x = string_of_float x
let of_int x = string_of_int x
let ignore _ = ""

let () =
    let parse_list = [
        ("-buildbot_prefix", Arg.String (fun x -> buildbot_prefix := x), 
        "The prefix to be used if the buildbot option is passed.");
        ("-buildbot_suffix", Arg.String (fun x -> buildbot_suffix := x), 
        "The suffix to be used if the buildbot option is passed.");
        ("-buildbot", Arg.Unit (fun () -> 
            buildbot := true; 
            print_string !buildbot_prefix),
        "Dump the rest of the input line for buildbot. This option turns \
        off all testing.");
        ("-inputfile", Arg.String (fun x -> 
            for_buildbot_output "-inputfile" ident x;
            files := x :: !files), 
        "An input file containing a list of files that should be replacing FILENAMEX");
        ("-error", Arg.Unit (fun () -> 
            for_buildbot_output "-error" ignore ();
            error_expected := true), 
        "Expect an error with normal program termination (no signals) from the poy_test.");
        ("-stderr", Arg.String (assign_opt "-stderr" ident mstderr),
        "The file where the reference stderr is located.");
        ("-stdout", Arg.String (assign_opt "-stdout" ident mstdout),
        "The file where the reference stdout is located.");
        ("-diff", Arg.String (assign_opt "-diff" ident mdiff),
        "The file where a reference output is located.");
        ("-ostderr", Arg.String (assign_opt "-ostderr" ident rmstderr),
        "The file where the reference stderr is to be stored.");
        ("-ostdout", Arg.String (assign_opt "-ostdout" ident rmstdout),
        "The file where the reference stdout is to be stored.");
        ("-odiff", Arg.String (assign_opt "-odiff" ident rmdiff),
        "The file where a reference output is to be stored.");
        ("-tempfile", Arg.String (assign "-tmpfile" ident temp),
        "The file where the stdout of the test run is to be stored. tmp.log by default");
        ("-message", Arg.String (assign "-message" ident message), 
        "The message to be printed out during the test");
        ("-command", Arg.String (assign "-command" ident command),
        "The command to be executed in this test");
        ("-costless", Arg.Float (fun x -> 
            for_buildbot_output "-costless" of_float x;
            check_cost_less := [x]), 
        "The cost of the final tree, if we are intended to do that");
        ("-cost", Arg.Float (fun x -> 
            for_buildbot_output "-cost" of_float x;
            check_cost := [x]), 
        "The cost of the final tree, if we are intended to do that");
        ("-costfile", Arg.String (assign_opt "-costfile" ident costfile),
        "A file containing the expected cost for each combination of input files");
        ("-costlessfile", Arg.String (assign_opt "-costlessfile" ident costlessfile),
        "A file containing the maximum expected cost for each combination of input files")
    ]
    in
    Arg.parse parse_list (fun _ -> ()) "test_line.ml [OPTIONS]*";
    if !buildbot then begin
        print_string !buildbot_suffix;
        print_newline ();
        exit 0
    end else ()

let _ = Unix.system "rm tmp_line*.*"

let () = (* We verify options that are mutually exclusive *)
    let excl = 
        [(!mstdout, !rmstdout); (!mstderr, !rmstderr); (!mdiff, !rmdiff)]
    in
    let excl2 = 
        [(!costfile, !check_cost); (!costlessfile, !check_cost_less)]
    in
    List.iter (function (Some _, Some _) ->
        failwith 
        "Sorry, you combine a std* or diff option with ostd* or odiff option"
        | _ -> ()) excl;
    List.iter (function (Some _, (_ :: _)) ->
        failwith 
        "Sorry, you combine a cost[less] and cost[less]file commands"
        | _ -> ()) excl2

let () = 
    let int_list target filename =
        match filename with
        | None -> ()
        | Some filename ->
            let ch = open_in filename in 
            let res = ref [] in
            try
                while true do
                    res := (float_of_string (input_line ch)) :: !res
                done;
            with End_of_file -> target := List.rev !res
    in
    int_list check_cost !costfile;
    int_list check_cost_less !costlessfile

let move_temp_to_report () =
    let _ = 
        Unix.system ("mv " ^ !temp ^ " " ^ (String.escaped !message)
        ^ ".err") 
    in
    ()

let pid = string_of_int (Unix.getpid ())
let default_stderr = "tmp_line" ^ pid ^ ".err"
let default_stdout = "tmp_line" ^ pid ^ ".out"
let default_report = "test" ^ pid ^ ".xml"

let append_all_output filename_fixer command =
    let append_output command (filename, redirector, default) =
        match filename, !rmstderr, !rmstdout, !mstderr, !mstdout with
        | None, None, None, None, None -> command
        | None, _, _, _, _ ->
                String.concat " " [ command; redirector; default ]
        | Some x, _, _, _, _ -> 
                let filename = filename_fixer x in
                String.concat " " [ command; redirector; filename ]
    in
    let lst =
        [(!rmstderr, "2>", default_stderr); (!rmstdout, ">", default_stdout)]
    in
    List.fold_left append_output command lst

let files = 
    List.fold_left (fun acc filename ->
        let lst = 
            let ch = open_in filename in 
            let res = ref [] in
            try
                while true do
                    res := (input_line ch) :: !res
                done;
                []
            with
            | End_of_file -> !res
        in
        (List.rev lst) :: acc) [] !files

let replacer (command, message, filename_fixer, counter) filename =
    let initial_filename = "FILENAME" ^ string_of_int counter in
    let replacer = 
        String.concat "" ["sed -e s,"; initial_filename; ","; filename; ","] 
    in
    let local_fix x = 
        Str.global_replace (Str.regexp initial_filename) filename x
    in
    let filename_fixer x = local_fix (filename_fixer x) in
    command ^ " | " ^ replacer, local_fix message, filename_fixer, counter + 1

let rec all_files_execution executer acc lst =
    match lst with
    | hd :: tl ->
            List.iter (fun filename ->
                all_files_execution executer (replacer acc filename) tl)
            hd
    | [] -> 
            let command, message, filename_fixer, _ = acc in
            let check_cost, cost_less =
                let get_cost_ref lst = 
                    match !lst with
                    | h :: t -> 
                            lst := t;
                            Some h
                    | [] -> None
                in
                get_cost_ref check_cost, get_cost_ref check_cost_less
            in
            executer (append_all_output filename_fixer) command message
            check_cost cost_less filename_fixer

let test_program =
    match Sys.os_type with
    | "Win32" -> " | poy_test.exe "
    | _ -> " | ./poy_test "

let () =
    let executer append_output command message check_cost check_cost_less
    filename_fixer =
        let () =
            match Unix.system ("rm -fr " ^ default_report) with
            | _ -> ()
        in
        let check_files a b =
            match a with
            | None -> true
            | Some a ->
                    let a = filename_fixer a
                    and b = filename_fixer b in
                    match Unix.system ("sed -i -e '/Estimated/d; /Automated Search/d' " ^ a) with
                    | Unix.WEXITED 0 -> 
                            (match Unix.system ("sed -i -e '/Estimated/d' " ^ b) with
                            | Unix.WEXITED 0 ->
                                    (match Unix.system ("diff -q " ^ a ^ " " ^
                                            b) with
                                    | Unix.WEXITED 0 -> true
                                    | _ -> false)
                            | _ -> false)
                   | _ -> false
        in
        let prefix = command ^ test_program in
        let execution_line =
            match check_cost, check_cost_less with
            | None, None -> prefix 
            | _, Some x -> prefix ^ " -cl " ^ string_of_float x
            | Some x, _ -> prefix ^ " -c " ^ string_of_float x
        in
        let execution_line = append_output execution_line in
        print_endline ("Executing " ^ execution_line);
        match Unix.system execution_line with
        | Unix.WEXITED n ->
                if n = 0 || !error_expected then
                    (* We finished cleanly, time to verify that things are correct
                    * *)
                    let () =
                        match !rmdiff with
                        | None -> ()
                        | Some x -> 
                                let x = filename_fixer x in
                                match Unix.system ("mv -f " ^ default_report ^ " " ^ x)
                                with
                                | Unix.WEXITED 0 -> ()
                                | _ ->
                                        Printf.printf 
                                        "FAILED: Could not generate reference %s\n"
                                        x
                    in
                    let res = 
                        (check_files !mstdout default_stdout) &&
                        (check_files !mstderr default_stderr) &&
                        (check_files !mdiff default_report)
                    in
                    if res then
                        Printf.printf "PASSED: %s\n%!" message
                    else
                        Printf.printf "FAILED: %s\n%!" message;
                else begin
                    Printf.printf "FAILED: %s\n%!" message;
                    exit_code := 1
                end
        | Unix.WSIGNALED x ->
                Printf.printf "FAILED: terminated with signal %d -- %s\n%!" x
                message;
                exit_code := 1
        | Unix.WSTOPPED x ->
                Printf.printf "FAILED: stopped with signal %d -- %s\n%!" x
                message;
                exit_code := 1
    in
    all_files_execution executer ("cat " ^ !command ^
    " | sed -e s,test.xml," ^ default_report ^ ",", !message, (fun x -> x), 1)
    files;
    exit !exit_code
