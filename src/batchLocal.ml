module Make (P : Scripting.S) : 
    Batch.S with type a = P.a with type b = P.b with type c = P.c = 
struct
    type a = P.a
    type b = P.b
    type c = P.c
	    
    type trees = (a, b) Ptree.p_tree Sexpr.t

    let info fmt = 
	Printf.ksprintf (Status.user_message Status.Information) fmt

    let generate_trees ~n_trees ~generate ~composer ~initial_state =
	let st = 
	    Status.create "Running Pipeline" (Some n_trees) "times" in
	let timer =
	    Timer.start () 
	in
	let rngs : Rng.t array =
	    Array.init n_trees (fun _ -> Rng.fork ()) 
	in
	let run0 = { initial_state with ScriptingTypes.stored_trees = `Empty }
	in
	let make_one (rng : Rng.t) : trees =
	    Rng.with_state rng (fun () ->
		info "Rng hash: %d" (Hashtbl.hash (Random.get_state ()));
		let run1 =
		    List.fold_left P.folder run0 generate 
		in
		info "Generated, now running compose";
		let run2 = List.fold_left P.folder run1 composer 
		in
		let adv = Status.get_achieved st + 1
		in
		let msg = Timer.status_msg (Timer.wall timer) adv n_trees 
		in
		let trees = run2.ScriptingTypes.stored_trees
		in
                Status.full_report ~adv ~msg st;
		info "Tree hash: %d" (Hashtbl.hash trees);
		trees)
	in
	let all_trees = 
	    Array.fold_left (fun trees rng -> 
		Sexpr.union (make_one rng) trees)
		`Empty
		rngs
	in
	begin
	    Status.finished st;
	    all_trees
	end
end
