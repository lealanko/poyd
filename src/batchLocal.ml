module Make (M : ScriptingTypes.S) : 
    Batch.S with module T = M.T =
struct
    module T = M.T
    include Batch.Defs(T)

    let info fmt = 
	Printf.ksprintf (Status.user_message Status.Information) fmt
    type trees = M.tree Sexpr.t

    let generate_trees {rng; n_trees; generate; composer; initial_state} =
	let st = 
	    Status.create "Running Pipeline" (Some n_trees) "times" in
	let timer =
	    Timer.start () 
	in
	let rngs : PoyRandom.t array =
	    Array.init n_trees (fun _ -> PoyRandom.fork rng) 
	in
	let run0 = { initial_state with ScriptingTypes.stored_trees = `Empty }
	in
	let make_one (subrng : PoyRandom.t) : trees =
	    PoyRandom.with_state subrng (fun () ->
		info "PoyRandom hash: %d" (Hashtbl.hash (PoyRandom.get_state ()));
		let run1 =
		    List.fold_left M.folder run0 generate 
		in
		info "Generated, now running compose";
		let run2 = List.fold_left M.folder run1 composer 
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
