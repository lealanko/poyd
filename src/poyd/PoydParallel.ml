open PoydDefs


module Servant = PoydServantStub
module Pool = PoydWorkerPool
module Client = PoydClientStub

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
    | Output of output list

type ('seed, 'result) dispatcher_state = {
    seeds : 'seed Dq.t;
    results : 'result Dq.t;
    n_running : int 
}

let dump_state {seeds; results; n_running} = 
    L.dbg "State: %d seeds, %d results, %d workers"
        (Dq.size seeds) (Dq.size results) n_running

let do_parallel 
        ~client
	~init_servant ~finalize_servant ~add_seed ~add_result ~get_result 
	~pool ~centralized_p seeds =
    let central_combiner = ref None
    in
    let dispatcher_mv = Lwt_mvar.create_empty ()
    in
    let worker_thread svt mvar =
        Servant.set_client svt client >>= fun () ->
	init_servant svt >>= fun ctx ->
	let notify_error seeds results thunk =
	    catch thunk
		(fun exn -> Lwt_mvar.put dispatcher_mv 
		    (WorkerFailed (mvar, seeds, results)) >>= fun () ->
		    fail exn)
	in
	let rec loop seeds results =
	    Lwt_mvar.put dispatcher_mv (WorkerReady (mvar, seeds, results))
	    >>= fun () ->
	    Lwt_mvar.take mvar >>= function
	      | AddSeed seed ->
                  let new_seeds = Dq.cons seed seeds
                  in
		  notify_error new_seeds results (fun () -> add_seed ctx seed)
                  >>= fun () ->
		  loop new_seeds results
	      | AddResult result ->
                  let new_results = Dq.cons result results
                  in
		  notify_error seeds new_results 
                      (fun () -> add_result ctx result) >>= fun () ->
		  loop seeds new_results
	      | GetResult ->
		  notify_error seeds results  (fun () -> get_result ctx) 
                  >>= fun result ->
		  notify_error seeds results (fun () -> Servant.get_output svt)
                  >>= fun output ->
                  Lwt_mvar.put dispatcher_mv (Output output) >>= fun () ->
		  loop Dq.empty (Dq.cons result Dq.empty)
	      | Free -> 
		  return ()
	in
	loop Dq.empty Dq.empty >>= fun () ->
	finalize_servant ctx >>= fun () ->
        L.dbg "Releasing parallel worker" >>= fun () ->
	Pool.put pool svt
    in
    let make_worker svt =
	let mv = Lwt_mvar.create_empty () 
	in
	detach (fun () -> worker_thread svt mv);
	mv
    in
    let state_str = function 
        | Return _ -> "Return"
        | Fail _ -> "Fail"
        | Sleep -> "Sleep"
    in
    let msg fmt = Printf.kfprintf (fun out ->
        Printf.fprintf out "\n%!") stderr fmt
    in
    let trace_cancel t = 
        msg "pre-cancel: %s" (state_str (state t));
        cancel t;
        msg "post-cancel: %s" (state_str (state t));
        
    in
    let request_servant pri = 
        pause () >>= fun () ->
         L.dbg "Requesting parallel worker" >>= fun () ->
        Pool.get pool pri >>= fun svt ->
	(* This needs to be atomic so it can't be canceled. 
	   Otherwise we might lose svt. *)
        catch (fun () ->
            msg "Received worker";
            (* L.dbg "Received worker" >>= fun () -> *)
            Lwt_mvar.put dispatcher_mv (NewServant svt))
            (fun exn -> 
                Pool.put pool svt >>= fun () ->
                fail exn) >>= fun () ->
        L.dbg "Sent worker to dispatcher"
    in
    let dispatch_event dst = function
	| WorkerReady (w, w_seeds, w_results) -> begin
            L.dbg "WorkerReady" >>= fun () ->
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
            L.dbg "WorkerFailed" >>= fun () ->
	    (match !central_combiner with
	    | Some cw when cw == w ->
		central_combiner := None
	    | _ -> ());
	    return { seeds = Dq.append w_seeds dst.seeds;
		     results = Dq.append w_results dst.results;
		     n_running = dst.n_running - 1 }
	end
	| NewServant svt -> begin
            L.dbg "NewServant" >>= fun () ->
	    let _ = make_worker svt in
	    return { dst with n_running = dst.n_running + 1 }
        end
        | Output o ->
            detach (fun () -> Client.execute_output client o);
            return dst
            
            
    in
    let rec dispatcher_loop dst =
        dump_state dst >>= fun () ->
	match Dq.front dst.results with
	| Some (res, results2) 
		when (Dq.is_empty dst.seeds && Dq.is_empty results2
		      && dst.n_running = 0) ->
	    return res
	| _ -> 
	    let req_tm = 
		if Dq.size dst.seeds > 0 || Dq.size dst.results > 1
		then Some (apply request_servant dst.n_running)
		else None
	    in
	    Lwt_mvar.take dispatcher_mv >>= fun event ->
            (match req_tm with
            | None -> ()
            | Some req_t -> trace_cancel req_t);
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
        PoydState.send state svt >>= fun () ->
        Servant.begin_script svt [`Store ([`Data], tmp_id)] >>= fun () ->
        return svt
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
	Servant.get_stored_trees svt >>= fun trees ->
        return (Phylo.normalize_trees trees)
    in
    let seeds = create_rngs state.PoydState.rng n
    in
    do_parallel ~client
	~init_servant ~finalize_servant ~add_seed ~add_result ~get_result 
	~pool ~centralized_p:false seeds >>= fun trees ->
    return { state with PoydState.run = 
	    { state.PoydState.run with stored_trees = trees } }



	      
let on_each_tree pool client state todo combine =
    let init_servant svt = 
        PoydState.send state svt >>= fun () ->
        Servant.begin_oneachtree svt todo combine >>= fun (iter, finish) ->
        return (svt, iter, finish)
    in
    let finalize_servant (_, _, finish) =
        finish ()
    in
    let add_seed (_, iter, _) (rng, tree) =
	L.info "Adding seed: rng %08x, tree %08x"
            (Hashtbl.hash rng) (Hashtbl.hash tree)
        >>= fun () ->
        iter rng tree
    in
    let add_result (svt, _, _) trees =
	L.info "Adding trees %08x" (Hashtbl.hash trees) >>= fun () ->
        Servant.add_stored_trees svt trees >>= fun () ->
        L.info "Trees sent, begin combine script" >>= fun () ->
        Servant.begin_script svt combine >>= fun () ->
        L.info "Script done"
    in
    let get_result (svt, _, _) =
	L.info "Getting trees" >>= fun () ->
	Servant.get_stored_trees svt >>= fun trees ->
        return (Phylo.normalize_trees trees) >>= fun trees ->
        L.info "Received trees %08x" (Hashtbl.hash trees) >>= fun () ->
        return trees
    in
    let trees = Sexpr.to_list state.PoydState.run.trees
    in
    let rngs = create_rngs state.PoydState.rng (List.length trees)
    in
    let seeds = BatList.map2 (fun a b -> (a, b)) rngs trees
    in
    Lwt_list.iter_s (fun (rng, tree) ->
        L.info "Seed: rng %08x, tree %08x" (Hashtbl.hash rng) (Hashtbl.hash tree))
        seeds >>= fun () ->
    do_parallel ~client
	~init_servant ~finalize_servant ~add_seed ~add_result ~get_result 
	~pool ~centralized_p:false seeds >>= fun trees ->
    return { state with PoydState.run = 
	    { state.PoydState.run with stored_trees = trees } }


let support pool client state meth =
    let (it, meth1) = Phylo.decompose_support meth state.PoydState.run 
    in
    let typ = match meth with
        | `Jackknife _ -> `Jackknife 
        | `Bootstrap _ -> `Bootstrap
    in
    let init_servant svt =
        PoydState.send state svt >>= fun () ->
        Servant.begin_support svt meth1 >>= fun (iter, finish) ->
        return (svt, iter, finish, ref None)
    in
    let finalize_servant (_, _, finish, _) = 
        finish ()
    in
    let add_seed (_, iter, _, _) rng =
	L.info "Adding seed: rng %08x" (Hashtbl.hash rng) >>= fun () ->
        iter rng 
    in
    (* We cheat a bit by combining results locally, since there's no real computation 
       involved. *)
    let add_result (_, _, _, r) s =
        r := Phylo.add_support !r s;
        return ()
    in
    let get_result (svt, _, _, r) =
	L.info "Getting support" >>= fun () ->
        Servant.get_support svt typ >>= fun sup ->
        L.info "Received support %08x" (Hashtbl.hash sup) >>= fun () ->
        return (Phylo.add_support !r sup)
    in
    L.info "Creating rng seeds for support, initial rng: %08x" 
        (Hashtbl.hash state.PoydState.rng) >>= fun () ->
    let seeds = create_rngs state.PoydState.rng it
    in
    do_parallel ~client
	~init_servant ~finalize_servant ~add_seed ~add_result ~get_result 
	~pool ~centralized_p:false seeds >>= fun res ->
    return { state with PoydState.run =
            match typ with
            | `Bootstrap -> { state.PoydState.run with 
                bootstrap_support = res }
            | `Jackknife -> { state.PoydState.run with
                jackknife_support = res } }

let bremer pool client state (`Bremer (local_optimum, build, _, _)) =
    fail (Failure "unimplemented")

    
let no_cont t =
    t >>= fun ret ->
    return (ret, [])

let run_parallel pool client state meth = match meth with
    | #Methods.support_method as meth ->
	no_cont (support pool client state meth)
    | #Methods.bremer_support as meth ->
	no_cont (bremer pool client state meth)
    | `OnEachTree (todo, combine) ->
	no_cont (on_each_tree pool client state todo combine)
    | `ParallelPipeline (times, todo, composer, continue) ->
	parallel_pipeline pool client state times todo composer >>= fun s ->
	return (s, continue)
