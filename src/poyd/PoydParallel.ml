open PoydDefs


module Servant = PoydServantStub
module Pool = PoydWorkerPool
module Client = PoydClientStub

module L = (val FundLog.make "PoydParallel" : FundLog.S)

module E = BatEnum

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
    | WorkerReady of ('seed, 'result) worker * 'seed list * 'result list
    | WorkerFailed of ('seed, 'result) worker * 'seed list * 'result list
    | NewServant of Servant.t
    | Output of output list

type ('seed, 'result) dispatcher_state = {
    seeds : 'seed list;
    results : 'result list;
    n_running : int 
}

type ('w, 'seed, 'result) map_worker = { 
    compute_seed : 'w -> 'seed -> 'result lwt;
    combine_results : 'result -> 'result -> 'result lwt;
}        

type ('w, 'seed, 'result) accum_worker = { 
    add_seed : 'w -> 'seed -> unit lwt;
    add_result : 'w -> 'result -> unit lwt;
    get_result : 'w -> 'result lwt;
}


let dump_state {seeds; results; n_running} = 
    L.dbg "State: %d seeds, %d results, %d workers"
        (List.length seeds) (List.length results) n_running

let notify_error d_mv w_mv seeds results thunk =
    catch thunk
	(fun exn -> 
            L.dbg "worker failed" >>= fun () ->
            Lwt_mvar.put d_mv 
	        (WorkerFailed (w_mv, seeds, results)) >>= fun () ->
	    fail exn)

let accum_worker ~add_seed ~add_result ~get_result svt ctx d_mv w_mv =
    let rec loop seeds results =
	Lwt_mvar.put d_mv (WorkerReady (w_mv, seeds, results))
	>>= fun () ->
	Lwt_mvar.take w_mv >>= function
	  | AddSeed seed -> begin
              let new_seeds = seed :: seeds
              in
	      notify_error d_mv w_mv new_seeds results 
                  (fun () -> add_seed ctx seed)
              >>= fun () ->
	      loop new_seeds results
          end
	  | AddResult result -> begin
              let new_results = result :: results
              in
	      notify_error d_mv w_mv seeds new_results 
                  (fun () -> add_result ctx result) >>= fun () ->
	      loop seeds new_results
          end
	  | GetResult -> begin
	      notify_error d_mv w_mv seeds results  
                  (fun () -> get_result ctx) 
              >>= fun result ->
	      notify_error d_mv w_mv seeds results
                  (fun () -> Servant.get_output svt)
              >>= fun output ->
              Lwt_mvar.put d_mv (Output output) >>= fun () ->
	      loop [] [result]
          end
	  | Free -> 
	      return ()
    in
    loop [] []

let map_worker ~compute_seed ~combine_result svt ctx d_mv w_mv =
    let add_result res mres = match mres with
        | None -> Some res
        | Some res2 -> Some (combine_result res res2)
    in
    let option_list o = BatList.of_enum (BatOption.enum o)
    in
    let rec loop mres =
	Lwt_mvar.put d_mv (WorkerReady (w_mv, [], option_list mres))
	>>= fun () ->
	Lwt_mvar.take w_mv >>= function
	  | AddSeed seed -> begin
	      notify_error d_mv w_mv [seed] (option_list mres)
                  (fun () -> compute_seed ctx seed)
              >>= fun result ->
	      loop (add_result result mres)
          end
	  | AddResult result -> 
	      loop (add_result result mres)
	  | GetResult -> 
              loop mres
	  | Free -> 
	      return ()
    in
    loop None

let do_parallel 
        ~client
	~init_servant ~finalize_servant ~work
	~pool seeds =
    let dispatcher_mv = Lwt_mvar.create_empty ()
    in
    let worker_thread svt mvar =
        L.dbg "Set client" >>= fun () ->
        Servant.set_client svt client >>= fun () ->
        L.dbg "Initialize worker" >>= fun () ->
	init_servant svt >>= fun ctx ->
        L.dbg "Beginning worker loop" >>= fun () ->
        work svt ctx dispatcher_mv mvar >>= fun () ->
        L.dbg "Finalize worker" >>= fun () ->
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
	    match dst.seeds, dst.results with
	    | seed :: seeds2, _ ->
		Lwt_mvar.put w (AddSeed seed) >>= fun () ->
		return { dst with seeds = seeds2 }
	    | [], result :: results2 -> begin
		Lwt_mvar.put w (AddResult result) >>= fun () ->
		return { dst with results = results2 }
	    end
	    | [], _ when w_seeds = [] && List.length w_results = 1 ->
		let [w_res] = w_results in
		Lwt_mvar.put w Free >>= fun () ->
		return { dst with results = w_res :: dst.results;
		    n_running = dst.n_running - 1 }
	    | [], _  -> 
		Lwt_mvar.put w GetResult >>= fun () ->
		return dst
	end
	| WorkerFailed (w, w_seeds, w_results) -> begin
            L.dbg "WorkerFailed" >>= fun () ->
	    return { seeds = w_seeds @ dst.seeds;
		     results = w_results @ dst.results;
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
	match dst.results with
	| [res] when (dst.seeds = [] && dst.n_running = 0) ->
	    return res
	| [] when (dst.seeds = [] && dst.n_running = 0) ->
            fail (Failure "no seeds?")
	| _ -> 
	    let req_tm = 
		if List.length dst.seeds > 0 || List.length dst.results > 1
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
    L.dbg "Begin dispatcher loop" >>= fun () ->
    dispatcher_loop { seeds = seeds; 
		      results = [];
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
	L.info "Got %d trees" (Sexpr.cardinal trees) >>= fun () ->
        return (Phylo.normalize_trees trees)
    in
    let seeds = create_rngs state.PoydState.rng n
    in
    do_parallel ~client
	~init_servant ~finalize_servant 
        ~work:(accum_worker ~add_seed ~add_result ~get_result)
	~pool seeds >>= fun trees ->
    L.dbg "Produced %d trees" (Sexpr.cardinal trees) >>= fun () ->
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
	~init_servant ~finalize_servant 
        ~work:(accum_worker ~add_seed ~add_result ~get_result)
	~pool seeds >>= fun trees ->
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
	~init_servant ~finalize_servant 
        ~work:(accum_worker ~add_seed ~add_result ~get_result)
	~pool seeds >>= fun res ->
    return { state with PoydState.run =
            match typ with
            | `Bootstrap -> { state.PoydState.run with 
                bootstrap_support = res }
            | `Jackknife -> { state.PoydState.run with
                jackknife_support = res } }

let bremer pool client state meth =
    let init_servant svt =
        Servant.begin_bremer svt state.PoydState.run meth >>= 
            fun (iter, finish) ->
        return (iter, finish)
    in
    let finalize_servant (_, finish) = 
        finish ()
    in
    let compute_seed (iter, _) (rng, tree) =
	L.info "Computing seed: rng %08x" (Hashtbl.hash rng) >>= fun () ->
        iter rng tree >>= fun stree ->
        return (`Single stree)
    in
    let combine_result trees1 trees2 =
        Sexpr.union trees1 trees2
    in
    let trees = Sexpr.to_list state.PoydState.run.trees
    in
    let rngs = create_rngs state.PoydState.rng (List.length trees)
    in
    let seeds = BatList.map2 (fun a b -> (a, b)) rngs trees
    in
    L.dbg "n_seeds: %d" (List.length seeds) >>= fun () ->
    do_parallel ~client
	~init_servant ~finalize_servant 
        ~work:(map_worker ~compute_seed ~combine_result)
	~pool seeds >>= fun res ->
    return { state with PoydState.run =
            { state.PoydState.run with bremer_support = res }}
    
let no_cont t =
    t >>= fun ret ->
    return (ret, [])

let release_svt reroot pool svt =
    (if reroot 
    then Servant.reroot svt 
    else return ()) >>= fun () ->
    L.trace (fun () -> PoydState.receive svt)
        "Receive state from servant" >>= fun state ->
    L.dbg "Trees in state: %d" (Sexpr.cardinal state.PoydState.run.trees)
    >>= fun () ->
    L.info "Releasing servant" >>= fun () ->
    Pool.put pool svt >>= fun () ->
    L.info "Released" >>= fun () ->
    return state

let run_parallel pool client svt meth = match meth with
    | #Methods.support_method as meth ->
        release_svt true pool svt >>= fun state ->
	no_cont (support pool client state meth)
    | #Methods.bremer_support as meth ->
        Servant.reroot svt >>= fun () ->
        release_svt true pool svt >>= fun state ->
	no_cont (bremer pool client state meth)
    | `OnEachTree (todo, combine) ->
        release_svt false pool svt >>= fun state ->
	no_cont (on_each_tree pool client state todo combine)
    | `ParallelPipeline (times, todo, composer, continue) ->
        release_svt false pool svt >>= fun state ->
	parallel_pipeline pool client state times todo composer >>= fun s ->
	return (s, continue)
