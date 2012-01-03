open PoydDefs

module Client = PoydClientStub

module L = (val FundLog.make "PoydServantImpl" : FundLog.S)

type t = unit

let thr = PoydPoy.thread

let output_dump out v = output_string out (dump v)

let current_output = Queue.create ()

let current_trees = Queue.create ()

let add_output o = 
    Queue.add o current_output

let servant = ()

let current_run = ref (Phylo.empty ())

let run_stack = Stack.create ()

let current_margin_id = ref 0

let get_name c =
    PoydUtil.get_procid ()

let output_status c k msg =
    PoydThread.callback thr (fun () ->
        match k with
        | Status.Output (f, _, _) ->
            add_output (OutputStatus (k, msg));
            return ()
        | _ ->
            Client.output_status c k msg)

let get_margin filename =
    decr current_margin_id;
    add_output (GetMargin (filename, !current_margin_id));
    !current_margin_id

let set_margin filename margin =
    add_output (SetMargin (filename, margin))

let remote_open_in c close_it opener fn =
    let path = FileStream.filename fn in
    (* XXX: awful kludge, need to at least make sure the temp file gets
       deleted. (On windows you cannot just unlink it after opening?) *)
    let file_contents = PoydThread.callback thr
        (fun () -> Client.request_file c path) in
    let (tmp_path, tmp_ch) = 
        Filename.open_temp_file ~mode:[Open_binary] "POY" ".input" in
    output_string tmp_ch file_contents;
    close_out tmp_ch;
    FileStream.(local_open_in.open_in_fn) close_it opener (`Local tmp_path)

let remote_explode_filenames c files =
    PoydThread.callback thr (fun () ->
        Client.explode_filenames c files)

let set_information_output filename =
    add_output (SetInformationOutput filename)

let current_client = ref None

let set_client _ c = PoydThread.run thr (fun () -> begin
    current_client := Some c;
    current_run := Phylo.empty ();
    Status.is_parallel 1 (Some (output_status c));
    FileStream.(set_current_open_in 
        { open_in_fn = fun (type t) -> remote_open_in c });
    StatusCommon.Files.get_margin_fn := get_margin;
    StatusCommon.Files.set_margin_fn := set_margin;
    StatusCommon.set_information_output_fn := set_information_output;
    PoyParser.set_explode_filenames_fn (remote_explode_filenames c)
end)
    

(*
let begin_script _ script = 
    L.dbg "RNG hash: %d" (Hashtbl.hash (PoyRandom.get_state ())) >>= fun () ->
    ccc (fun k ->
        detach (fun () ->
            PoydThread.run thr <| fun () -> begin
                PoydThread.callback thr (fun () -> 
                    L.dbg "Wakeup return continuation" >>= fun () ->
                    wakeup k ();
                    L.dbg "Woke up?" >>= fun () ->
                    return ());
                let new_run = List.fold_left Phylo.folder !current_run script in
                current_run := new_run
            end))
*)

let begin_script _ script = 
    PoydThread.run thr <| fun () ->
        let new_run = List.fold_left Phylo.folder !current_run script in
        current_run := new_run

let final_report _ =
    PoydThread.run thr (fun () ->
        Phylo.final_report !current_run)

let set_rng _ rng = PoydThread.run thr (fun () ->
    PoyRandom.set_state rng
)

let get_rng _ = PoydThread.run thr (fun () ->
    PoyRandom.get_state ()
)

let get f = PoydThread.run thr 
    <| fun () -> f !current_run
let set f = PoydThread.run thr 
    <| fun () -> current_run := f !current_run

let set_run _ r = set <| fun _ -> r 
let get_run _ = get <| fun r -> r
let set_data _ data = set <| fun r -> { r with data }
let get_data _ = get <| fun r -> r.data
let set_trees _ trees = set <| fun r -> { r with trees }
let set_stored_trees _ stored_trees = set <| fun r -> { r with stored_trees }
let add_trees _ trees = set <| fun r -> 
    { r with trees = Sexpr.union r.trees trees }
let add_stored_trees _ stored_trees = set <| fun r -> 
    {r with stored_trees = Sexpr.union r.stored_trees stored_trees}
let get_trees _ = get <| fun r -> r.trees
let get_stored_trees _ = get <| fun r -> r.stored_trees

let get_output _ = return (BatList.of_enum (BatQueue.enum current_output))

let clear_output _ = Queue.clear current_output; return ()

let begin_oneachtree _ dosomething mergingscript = 
    (PoydThread.run thr <| fun () ->
        let (name, run) = 
            Phylo.begin_on_each_tree Phylo.folder dosomething !current_run
        in
        current_run := run;
        name) >>= fun name ->
    let iter rng tree =
        set <| Phylo.iter_on_each_tree 
                Phylo.folder name dosomething mergingscript rng tree
    in
    let finish () =
        set <| Phylo.end_on_each_tree Phylo.folder name
    in
    return (iter, finish)

let push_run _ = 
    Stack.push !current_run run_stack

let pop_run _ = PoydThread.run thr <| fun () ->
    current_run := Stack.pop run_stack

let save_original_trees _ = set <| 
        fun r -> { r with original_trees = r.trees }

let clear_original_trees _ = set <| 
        fun r -> { r with original_trees = `Empty }

let begin_support _ meth = (set <| Phylo.begin_support meth) >>= fun () ->
    let iter rng =
        set <| Phylo.iter_support meth rng
    in
    let finish () = return ()
    in
    return (iter, finish)

let get_support s typ = get <|
        fun r -> match typ with
        | `Bootstrap -> r.bootstrap_support
        | `Jackknife -> r.jackknife_support

let get_bremer s = fail (Failure "unimplemented")
