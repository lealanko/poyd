module Make (P : ScriptingTypes.S) : 
    Batch.S with module T = P =
struct
    module T = P
    include Batch.Base(T)
    type trees = (a, b) Ptree.p_tree Sexpr.t

    let info fmt = 
	Printf.ksprintf (Status.user_message Status.Information) fmt

    let generate_trees {rng; n_trees; generate; composer; initial_state} =
	let st = 
	    Status.create "Running Pipeline" (Some n_trees) "times" in
	let timer =
	    Timer.start () 
	in
	let rngs : Rng.t array =
	    Array.init n_trees (fun _ -> Rng.fork rng) 
	in
	let run0 = { initial_state with ScriptingTypes.stored_trees = `Empty }
	in
	let make_one (subrng : Rng.t) : trees =
	    Rng.with_state subrng (fun () ->
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
	    Array.fold_left (fun trees subrng -> 
		Sexpr.union (make_one subrng) trees)
		`Empty
		rngs
	in
	begin
	    Status.finished st;
	    { trees = all_trees }
	end
end
