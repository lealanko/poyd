let running_counter = ref 0
let max_processes = ref 1
let input_file = ref ""

let () =
    let parse_list = [
        ("-scriptsfile", Arg.String (fun x -> input_file := x),
        "Input file containing the list of scripts to run");
        ("-p", Arg.Int (fun x -> if x > 0 then max_processes := x),
        "Number of processes to be run concurrently")
    ] in
    Arg.parse parse_list (fun _ -> ()) "concurrent_test.ml [OPTIONS]"


let concatenate pid =
    let pid = string_of_int pid in
    let _ = 
        Unix.system ("cat test_all" ^ pid ^ ".log >> test_all.log" ) 
    in
    ()

let prepare_line line pid = 
    let pid = string_of_int pid in
    "ocaml unix.cma str.cma test_line.ml " ^ line ^ " > test_all" ^ pid ^
    ".log"

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
    let () = decr running_counter in
    let pid = 
        if !max_processes > 1 then
            let pid, _ = Unix.wait () in
            pid
        else Unix.getpid () 
    in
    concatenate pid


let not_empty line = line <> ""

let rec process_list_of_tasks lst =
    match lst with
    | h :: t when !running_counter < !max_processes-> 
            if not_empty h then fork_and_execute h;
            collect_results ();
            process_list_of_tasks t
    | h :: t -> 
            collect_results ();
            process_list_of_tasks t
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
