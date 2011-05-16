open PoydDefs

module Client = PoydClientStub

module L = (val FundLog.make "PoydServantImpl" : FundLog.S)


type t = unit

let servant = ()

let current_run = ref (Phylo.empty ())

let thr = PoydPoy.thread

let get_name c =
    PoydUtil.get_procid ()

let remote_output_status c k msg =
    PoydThread.callback thr (fun () ->
        Client.output_status c k msg)

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

let current_client = ref None

let set_client _ c = PoydThread.run thr (fun () -> begin
    current_client := Some c;
    current_run := Phylo.empty ();
    Status.is_parallel 1 (Some (remote_output_status c));
    FileStream.(set_current_open_in 
        { open_in_fn = fun (type t) -> remote_open_in c });
    PoyParser.set_explode_filenames_fn (remote_explode_filenames c)
end)
    

let execute_script _ script = 
    L.dbg "RNG hash: %d" (Hashtbl.hash (Random.get_state ())) >>= fun () ->
    PoydThread.run thr (fun () -> begin
    let new_run = Phylo.run ~start:!current_run script in
    current_run := new_run
end)

let set_rng _ rng = PoydThread.run thr (fun () ->
    Random.set_state rng
)

let get_rng _ = PoydThread.run thr (fun () ->
    Random.get_state ()
)

let get f = PoydThread.run thr (fun () -> f !current_run)
let set f = PoydThread.run thr (fun () -> current_run := f !current_run)

let set_run _ r = set (fun _ -> r)
let get_run _ = get (fun r -> r)
let set_data _ data = set (fun r -> { r with data })
let get_data _ = get (fun r -> r.data)
let set_trees _ trees = set (fun r -> { r with trees })
let set_stored_trees _ stored_trees = set (fun r -> { r with stored_trees })
let get_trees _ = get (fun r -> r.trees)
let get_stored_trees _ = get (fun r -> r.stored_trees)


