open PoydDefs


module Servant = PoydServantStub
module Pool = PoydWorkerPool

module L = (val FundLog.make "PoydParallel" : FundLog.S)

module E = BatEnum

module Dq = BatDeque


(* Parallel stuff *)

(* Parallelization algorithm:
   While seed tasks remain, request new workers constantly.

   Upon WorkerReady, if we have a pending Seed task, send that.

   If not, but we have a pending Combine task and the worker is not
   initial, send the task.

   Otherwise, if the worker is not initial, receive the current result.

   If the worker is initial and we have a pending combine task but 
   
*)
type ('seed, 'result) command =
    | AddSeed of 'seed
    | AddResult of 'result
    | GetResult
    | Free

type ('seed, 'result) worker = ('seed, 'result) command Lwt_mvar.t

type ('seed, 'result) event =
    | WorkerReady of ('seed, 'result) worker * 'seed Dq.t * 'result Dq.t
    | WorkerFailed of ('seed, 'result) worker * 'seed Dq.t * 'result Dq.t
    | NewServant of Servant.t

type ('seed, 'result) dispatcher_state = {
    seeds : 'seed Dq.t;
    results : 'result Dq.t;
    n_running : int 
}

let do_parallel 
	~init_servant ~finalize_servant ~add_seed ~add_result ~get_result 
	~pool ~centralized_p seeds =
    let central_combiner = ref None
    in
    let dispatcher_mv = Lwt_mvar.create_empty ()
    in
    let worker_thread svt mvar =
	let rec loop seeds results =
	    let notify_error thunk =
		catch thunk
		    (fun exn -> Lwt_mvar.put dispatcher_mv 
			(WorkerFailed (mvar, seeds, results)) >>= fun () ->
			fail exn)
	    in
	    Lwt_mvar.put dispatcher_mv (WorkerReady (mvar, seeds, results))
	    >>= fun () ->
	    Lwt_mvar.take mvar >>= function
	      | AddSeed seed ->
		  notify_error (fun () -> add_seed svt seed) >>= fun () ->
		  loop (Dq.cons seed seeds) results
	      | AddResult result ->
		  notify_error (fun () -> add_result svt result) >>= fun () ->
		  loop seeds (Dq.cons result results)
	      | GetResult ->
		  notify_error (fun () -> get_result svt) >>= fun result ->
		  loop Dq.empty (Dq.cons result Dq.empty)
	      | Free -> 
		  return ()
	in
	init_servant svt >>= fun () ->
	loop Dq.empty Dq.empty >>= fun () ->
	finalize_servant svt >>= fun () ->
	Pool.put pool svt
    in
    let make_worker svt =
	let mv = Lwt_mvar.create_empty () 
	in
	detach (fun () -> worker_thread svt mv);
	mv
    in
    let request_servant pri = 
        L.dbg "requesting new servant from pool" >>= fun () ->
        Pool.get pool pri >>= fun svt ->
	(* This needs to be atomic so it can't be canceled. 
	   Otherwise we might lose svt. *)
        return (NewServant svt)
    in
    let dispatch_event dst = function
	| WorkerReady (w, w_seeds, w_results) -> begin
	    match Dq.front dst.seeds, Dq.front dst.results with
	    | Some (seed, seeds2), _ -> 
		Lwt_mvar.put w (AddSeed seed) >>= fun () ->
		return { dst with seeds = seeds2 }
	    | None, Some (result, results2)
		when (match !central_combiner with
		| None -> true
		| Some cw when cw == w -> true
		| _ -> false) -> begin
		    (if centralized_p 
		     then central_combiner := Some w);
		    Lwt_mvar.put w (AddResult result) >>= fun () ->
		    return { dst with results = results2 }
		end
	    | None, _ when Dq.is_empty w_seeds && Dq.size w_results = 1 ->
		let [w_res] = Dq.to_list w_results in
		Lwt_mvar.put w Free >>= fun () ->
		return { dst with results = Dq.cons w_res dst.results;
		    n_running = dst.n_running - 1 }
	    | None, _  -> 
		Lwt_mvar.put w GetResult >>= fun () ->
		return dst
	end
	| WorkerFailed (w, w_seeds, w_results) -> begin
	    (match !central_combiner with
	    | Some cw when cw == w ->
		central_combiner := None
	    | _ -> ());
	    return { seeds = Dq.append w_seeds dst.seeds;
		     results = Dq.append w_results dst.results;
		     n_running = dst.n_running - 1 }
	end
	| NewServant svt ->
	    let _ = make_worker svt in
	    return { dst with n_running = dst.n_running + 1 }
    in
    let rec dispatcher_loop dst =
	match Dq.front dst.results with
	| Some (res, results2) 
		when (Dq.is_empty dst.seeds && Dq.is_empty results2
		      && dst.n_running = 0) ->
	    return res
	| _ -> 
	    let req_t = 
		if Dq.size dst.seeds > 0 || Dq.size dst.results > 1
		then request_servant dst.n_running
		else halt ()
	    in
	    let event_t = match state req_t with
		| Sleep -> pick [req_t; Lwt_mvar.take dispatcher_mv]
		| _ -> req_t
	    in
	    event_t >>= fun event ->
	    dispatch_event dst event >>= fun new_dst ->
	    dispatcher_loop new_dst
    in
    dispatcher_loop { seeds = Dq.of_list seeds; 
		      results = Dq.empty; 
		      n_running = 0 }
	
let create_rngs rng n =
    PoyRandom.with_state rng (fun () ->
	BatList.init n (fun _ -> PoyRandom.fork ()))

let gensym_count = ref (-1)

let gensym () =
    incr gensym_count;
    Printf.sprintf "__poy_id_%d" !gensym_count

let parallel_pipeline pool client state n todo combine =
    let tmp_id = gensym ()
    in
    let init_servant svt = 
        Servant.set_client svt client >>= fun () ->
        PoydState.send state svt >>= fun () ->
        Servant.begin_script svt [`Store ([`Data], tmp_id)]
    in
    let finalize_servant svt =
	Servant.begin_script svt [`Discard ([`Data], tmp_id)]
    in
    let build_script = `Set ([`Data], tmp_id) :: todo @ combine 
    in
    let add_seed svt rng =
        L.info "Adding rng %08x" (Hashtbl.hash rng) >>= fun () ->
        Servant.set_rng svt rng >>= fun () ->
	L.info "Begin build script" >>= fun () ->
        Servant.begin_script svt build_script
    in
    let add_result svt trees =
	L.info "Adding trees" >>= fun () ->
        Servant.add_stored_trees svt trees >>= fun () ->
        L.info "Trees sent, begin combine script" >>= fun () ->
        Servant.begin_script svt combine >>= fun () ->
        L.info "Script done"
    in
    let get_result svt =
	L.info "Getting trees" >>= fun () ->
	Servant.get_stored_trees svt
    in
    let seeds = create_rngs state.PoydState.rng n
    in
    do_parallel 
	~init_servant ~finalize_servant ~add_seed ~add_result ~get_result 
	~pool ~centralized_p:false seeds >>= fun trees ->
    return { state with PoydState.run = 
	    { state.PoydState.run with stored_trees = trees } }



	      
