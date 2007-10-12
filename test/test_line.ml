let command = ref ""
let message = ref ""
let check_cost = ref None
let check_cost_less = ref None
let temp = ref "tmp.log"

let assign item v = item := v

let () =
    let parse_list = [
        ("-tempfile", Arg.String (assign temp),
        "The file where the stdout of the test run is to be stored. tmp.log by default");
        ("-message", Arg.String (assign message), 
        "The message to be printed out during the test");
        ("-command", Arg.String (assign command),
        "The command to be executed in this test");
        ("-costless", Arg.Int (fun x -> check_cost_less := Some x), 
        "The cost of the final tree, if we are intended to do that");
        ("-cost", Arg.Int (fun x -> check_cost := Some x), 
        "The cost of the final tree, if we are intended to do that")]
    in
    Arg.parse parse_list (fun _ -> ()) "test_line.ml [OPTIONS]*"

let move_temp_to_report () =
    let _ = 
        Unix.system ("mv " ^ !temp ^ " " ^ (String.escaped !message)
        ^ ".err") 
    in
    ()

let () =
    let prefix = !command ^ " | ./poy_test " in
    let execution_line =
        match !check_cost, !check_cost_less with
        | None, None -> prefix ^ " &> " ^ !temp
        | _, Some x -> prefix ^ " -cl " ^ string_of_int x
        | Some x, _ -> prefix ^ " -c " ^ string_of_int x
    in
    print_endline "Executing ./poy_test";
    match Unix.system execution_line with
    | Unix.WEXITED 0 -> 
            Printf.printf "PASSED: %s\n%!" !message
    | Unix.WEXITED n ->
            move_temp_to_report ();
            Printf.printf "FAILED: %s\n%!" !message
    | Unix.WSIGNALED x ->
            move_temp_to_report ();
            Printf.printf "FAILED: terminated with signal %d -- %s\n%!" x !message
    | Unix.WSTOPPED x ->
            move_temp_to_report ();
            Printf.printf "FAILED: stopped with signal %d -- %s\n%!" x !message
