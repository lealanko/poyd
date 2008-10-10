let running_counter = ref 0
let max_processes = ref 1
let input_file = ref ""
let buildbot = ref false
let buildbot_prefix = ref ""
let buildbot_suffix = ref ""

let set_buildbot_mode () = 
    buildbot := true;
    buildbot_suffix := "]))";
    match !input_file with
    | "cost_tests" -> 
            buildbot_prefix := 
                "cost_calculation_tests.append (generate_test ([";
    | "search_tests" ->
            buildbot_prefix := "search_tests.append (generate_test ([";
    | "bucket_tests" -> 
            buildbot_prefix :=
                "append_by_bucket (command_tests, generate_test (["
    | x -> 
            prerr_string "Unknown mode for file: ";
            prerr_string x;
            prerr_newline ();
            exit 1


let () =
    let parse_list = [
        ("-buildbot", Arg.Unit set_buildbot_mode, 
        "Don't run the test but dump the buildbot ready file using prefix and \
        suffix according to the following modes: costcalculation, search, \
        bucket.");
        ("-scriptsfile", Arg.String (fun x -> input_file := x),
        "Input file containing the list of scripts to run");
        ("-p", Arg.Int (fun x -> if x > 0 then max_processes := x),
        "Number of processes to be run concurrently")
    ] in
    Arg.parse parse_list (fun _ -> ()) "concurrent_test.ml [OPTIONS]"


let concatenate pid =
    if not !buildbot then
        let pid = string_of_int pid in
        let _ = 
            Unix.system ("cat test_all" ^ pid ^ ".log >> test_all.log" ) 
        in
        ()
    else ()

let prepare_line line pid = 
    let executable =
        let executable = "ocaml unix.cma str.cma test_line.ml " in
        if !buildbot then 
            Printf.sprintf "%s -buildbot_prefix \"%s\" -buildbot_suffix \"%s\" -buildbot " 
            executable !buildbot_prefix !buildbot_suffix 
        else
            executable
    in
    let output_redirection =
        if !buildbot then " >> buildbot.txt "
        else " > test_all" ^ string_of_int pid ^ ".log"
    in
    executable ^ line ^ output_redirection

let fork_and_execute line =
    let execute pid = 
        let pid = Unix.getpid () in
        let line = prepare_line line pid in
        let _ = Unix.system line in
        ()
    in
    if !max_processes = 1 then 
        let () = incr running_counter in
        execute () 
    else
        let code = Unix.fork () in
        if code = 0 then (* We are the child, we proceed to execute the line *)
            let () = execute () in 
            exit 0
        else 
            incr running_counter

let collect_results () =
    if !max_processes > 1 && !running_counter > 0 then
        let pid, _ = Unix.wait () in
        let () = decr running_counter in
        concatenate pid
    else if !running_counter > 0 && !max_processes = 1 then
        let () = decr running_counter in
        concatenate (Unix.getpid ())
    else ()


let not_empty line = line <> ""

let rec process_list_of_tasks lst =
    match lst with
    | h :: t when !running_counter < !max_processes-> 
            if not_empty h then fork_and_execute h;
            process_list_of_tasks t
    | h :: t -> 
            collect_results ();
            process_list_of_tasks lst
    | [] when !running_counter > 0 -> 
            collect_results ();
            process_list_of_tasks []
    | [] -> ()

let rec get_list_of_tasks file =
    let ch = open_in file in
    let lst = ref [] in
    try while true do
        let line = input_line ch in
        lst := line :: !lst;
    done;
    [] 
    with End_of_file -> List.rev !lst

let () = process_list_of_tasks (get_list_of_tasks !input_file)
