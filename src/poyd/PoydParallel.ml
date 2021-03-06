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
    | AbortWorker

type ('seed, 'result) worker = ('seed, 'result) command Lwt_mvar.t

type wstatus = Ready | Aborted | Failed

type ('seed, 'result) wstate = {
    results : 'result list;
    seeds : 'seed list;
}

let init_wstate = {
    results = [];
    seeds = [];
}

type ('seed, 'result) event =
    | WorkerReady of ('seed, 'result) worker
    | WorkerDone of ('seed, 'result) worker
    | Add of 'seed list * 'result list
    | NewServant of Servant.t
    | Output of output list
    | AbortTask of exn

type ('seed, 'result) dispatcher_state = {
    d_seeds : 'seed list;
    d_results : 'result list;
    n_running : int;
    exn : exn option;
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


let dump_state {d_seeds; d_results; n_running} = 
    L.dbg "State: %d seeds, %d results, %d workers"
        (List.length d_seeds) (List.length d_results) n_running


let worker add_seed add_result get_result svt d_mv w_mv =
    Servant.get_name svt >>= fun name ->
    let rec loop wst =
        let rec finalize () =
            check (fun () ->
                L.dbg "Getting output" >>= fun () ->
                Servant.get_output svt >>= fun output ->
                L.dbg "Getting result" >>= fun () ->
                get_result wst >>= fun wst' ->
                Lwt_mvar.put d_mv (Output output) >>= fun () ->
                return wst')
                (function 
                  | Abort -> fail (Failure("impossible"))
                  | exn -> fail exn) >>= fun wst' ->
            Lwt_mvar.put d_mv (Add(wst'.seeds, wst'.results))
        and check thunk hdl =
            on_exn (fun () -> on_exn thunk hdl) (function
              | Abort -> begin
                  L.notice "Servant %s terminating" name >>= fun () ->
                  finalize ()
              end
              | ConnectionError _ | RouteError -> begin
                  L.error "Servant %s failed" name >>= fun () ->
                  Lwt_mvar.put d_mv (Add (wst.seeds, wst.results))
              end
              | exn ->
                  (* All other exceptions were relayed by the actual computation,
                     and indicate that the task itself failed *)
                  L.error "Task failed" >>= fun () ->
                  Lwt_mvar.put d_mv (AbortTask exn))
        and next wst = 
	    Lwt_mvar.put d_mv (WorkerReady w_mv) >>= fun () ->
            loop wst
        in
        Lwt_mvar.take w_mv >>= function
	  | AddSeed seed -> begin
              L.trace_ (fun () ->
                  check (fun () -> add_seed wst seed)
                      (fun exn -> 
                          Lwt_mvar.put d_mv (Add ([seed], []))))
                  "AddSeed"
              >>= next
          end
	  | AddResult result -> begin
              L.trace_ (fun () -> check 
                  (fun () -> add_result wst result)
                  (fun exn -> Lwt_mvar.put d_mv (Add ([], [result]))))
                  "AddResult"
              >>= next
          end
	  | GetResult ->
              L.trace_ (fun () -> finalize ())
                  "GetResult"
          | AbortWorker ->
              L.trace_ (fun () -> return ())
                  "AbortWorker"
    in
    finalize (fun () -> loop init_wstate)
        (fun () ->
            Lwt_mvar.put d_mv (WorkerDone w_mv))
    

let accum_worker ~add_seed ~add_result ~get_result ctx =
    let add_s wst seed = 
        add_seed ctx seed >>= fun () ->
        return {wst with seeds = seed :: wst.seeds }
    and add_r wst result =
        add_result ctx result >>= fun () ->
        return {wst with results = result :: wst.results }
    and get_r wst = 
        get_result ctx >>= fun result ->
        return {seeds = []; results = [result] }
    in
    worker add_s add_r get_r

let map_worker ~compute_seed ~combine_result ctx =
    let add_s wst seed = 
        compute_seed ctx seed >>= fun result ->
        return { wst with results = result :: wst.results }
    and add_r wst result =
        return { wst with results = result :: wst.results }
    and get_r wst = 
        return { wst with results = match wst.results with
        | [] -> []
        | r :: rs -> [List.fold_left combine_result r rs] }
    in
    worker add_s add_r get_r

let do_parallel 
        ~client
	~init_servant ~finalize_servant ~work
	~pool seeds =
    let dispatcher_mv = Lwt_mvar.create_empty ()
    in
    let worker_thread svt mvar =
        L.dbg "Set client" >>= fun () ->
        Servant.set_client svt (Some client) >>= fun () ->
        result (fun () ->
            L.trace_ (fun () -> init_servant svt)
                "Initialize worker" >>= fun ctx ->
            L.trace_ (fun () -> work ctx svt dispatcher_mv mvar)
                "Worker loop" >>= fun () ->
            L.trace_ (fun () -> finalize_servant ctx)
                "Finalize worker") >>= fun res ->
        L.trace_ (fun () -> Servant.set_client svt None)
            "Releasing parallel worker" >>= fun () ->
        match res with
        | Bad (ConnectionError _ | RouteError | Abort as exn) ->
            Servant.get_name svt >>= fun name ->
            L.info "Discarding finished servant %s" name >>= fun () ->
            fail exn
        | res ->
	    Pool.put pool svt >>= fun () ->
            run_result res
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
    let msg fmt = Printf.ifprintf () (fmt ^^ "\n%!")
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
        return (NewServant svt)
    in
    let rec dispatch_event dst = function
	| WorkerReady (w) -> begin
            L.dbg "WorkerReady" >>= fun () ->
	    match dst with
            | { exn = Some _ } ->
                Lwt_mvar.put w AbortWorker >>= fun () ->
                return dst
	    | { d_seeds = seed :: seeds2 } ->
		Lwt_mvar.put w (AddSeed seed) >>= fun () ->
		return { dst with d_seeds = seeds2 }
	    | { d_results = result :: results2 } -> begin
		Lwt_mvar.put w (AddResult result) >>= fun () ->
		return { dst with d_results = results2 }
	    end
	    | _ -> 
		Lwt_mvar.put w GetResult >>= fun () ->
		return dst
	end
        | Add (w_seeds, w_results) -> begin
            L.dbg "Add" >>=  fun () ->
	    return { dst with 
                d_seeds = w_seeds @ dst.d_seeds;
		d_results = w_results @ dst.d_results }
        end
	| WorkerDone (w) -> begin
            L.dbg "WorkerDone" >>= fun () ->
	    return { dst with n_running = dst.n_running - 1 }
	end
	| NewServant svt -> begin
            L.dbg "NewServant" >>= fun () ->
	    let w = make_worker svt 
            in
            let dst_ = { dst with n_running = dst.n_running + 1 }
            in
            dispatch_event dst_ (WorkerReady w)
        end
        | Output o -> begin 
            L.dbg "Output" >>= fun () ->
            detach (fun () -> Client.execute_output client o);
            return dst
        end
        | AbortTask exn -> 
            L.dbg "AbortTask" >>= fun () ->
            return { dst with exn = Some exn }
    in
    let rec dispatcher_loop dst =
        dump_state dst >>= fun () ->
        match dst with
        | { n_running = 0; exn = Some exn } ->
            fail exn
        | { n_running = 0; d_seeds = []; d_results = [res] } ->
	    return res
        | { n_running = 0; d_seeds = []; d_results = [] } ->
            fail (Failure "no seeds?")
	| _ -> 
	    let req_tm = match dst with
                | { exn = Some _ } -> []
                | { d_seeds = _ :: _ } 
                | { d_results = _ :: _ :: _ } ->
                    [Lwt.apply request_servant dst.n_running]
		| _ -> []
	    in
	    pick ([Lwt_mvar.take dispatcher_mv] @ req_tm) >>= fun event ->
	    dispatch_event dst event >>= fun new_dst ->
	    dispatcher_loop new_dst
    in
    L.dbg "Begin dispatcher loop" >>= fun () ->
    let t = dispatcher_loop { 
        d_seeds = seeds; 
	d_results = [];
	n_running = 0;
        exn = None;
    }
    in
    on_exn (fun () -> protected t)
        (function
          | Canceled -> begin
              L.dbg "Parallel task canceled, sending AbortTask" >>= fun () ->
              Lwt_mvar.put dispatcher_mv (AbortTask Canceled)
          end
          | _ -> return ())
	
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
        L.trace_ (fun () -> Servant.set_rng svt rng)
            "Adding rng %08x" (Hashtbl.hash rng) >>= fun () ->
        L.trace_ (fun () -> Servant.begin_script svt build_script)
            "Begin build script"
    in
    let add_result svt trees =
        L.trace_ (fun () -> Servant.add_stored_trees svt trees)
	    "Adding trees" >>= fun () ->
        L.trace_ (fun () -> Servant.begin_script svt combine)
            "Trees sent, begin combine script"
    in
    let get_result svt =
	L.dbg "Getting trees" >>= fun () ->
	Servant.get_stored_trees svt >>= fun trees ->
	L.dbg "Got %d trees" (Sexpr.cardinal trees) >>= fun () ->
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
	    { state.PoydState.run with stored_trees = trees }
           }



	      
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
	L.dbg "Adding seed: rng %08x, tree %08x"
            (Hashtbl.hash rng) (Hashtbl.hash tree)
        >>= fun () ->
        iter rng tree
    in
    let add_result (svt, _, _) trees =
	L.dbg "Adding trees %08x" (Hashtbl.hash trees) >>= fun () ->
        Servant.add_stored_trees svt trees >>= fun () ->
        L.dbg "Trees sent, begin combine script" >>= fun () ->
        Servant.begin_script svt combine >>= fun () ->
        L.dbg "Script done"
    in
    let get_result (svt, _, _) =
	L.dbg "Getting trees" >>= fun () ->
	Servant.get_stored_trees svt >>= fun trees ->
        return (Phylo.normalize_trees trees) >>= fun trees ->
        L.dbg "Received trees %08x" (Hashtbl.hash trees) >>= fun () ->
        return trees
    in
    let trees = Sexpr.to_list state.PoydState.run.trees
    in
    let rngs = create_rngs state.PoydState.rng (List.length trees)
    in
    let seeds = BatList.map2 (fun a b -> (a, b)) rngs trees
    in
    Lwt_list.iter_s (fun (rng, tree) ->
        L.dbg "Seed: rng %08x, tree %08x" (Hashtbl.hash rng) (Hashtbl.hash tree))
        seeds >>= fun () ->
    do_parallel ~client
	~init_servant ~finalize_servant 
        ~work:(accum_worker ~add_seed ~add_result ~get_result)
	~pool seeds >>= fun trees ->
    return { state with PoydState.run = 
	    { state.PoydState.run with trees;
                stored_trees = `Empty;
                original_trees = `Empty
            }
           }


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
	L.dbg "Adding seed: rng %08x" (Hashtbl.hash rng) >>= fun () ->
        iter rng 
    in
    (* We cheat a bit by combining results locally, since there's no real computation 
       involved. *)
    let add_result (_, _, _, r) s =
        r := Phylo.add_support !r s;
        return ()
    in
    let get_result (svt, _, _, r) =
	L.dbg "Getting support" >>= fun () ->
        Servant.get_support svt typ >>= fun sup ->
        L.dbg "Received support %08x" (Hashtbl.hash sup) >>= fun () ->
        return (Phylo.add_support !r sup)
    in
    L.dbg "Creating rng seeds for support, initial rng: %08x" 
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
	L.dbg "Computing seed: rng %08x" (Hashtbl.hash rng) >>= fun () ->
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

let run_parallel pool client svt meth = 
    let release_svt () =
        L.trace_ (fun () -> PoydState.receive svt)
            "Receive state from servant" >>= fun state ->
        L.trace_ (fun () -> Servant.get_output svt)
            "Receive output from servant" >>= fun output ->
        Client.execute_output client output >>= fun () ->
        L.dbg "Trees in state: %d" (Sexpr.cardinal state.PoydState.run.trees)
        >>= fun () ->
        Servant.set_client svt None >>= fun () ->
        Pool.put pool svt >>= fun () ->
        return state
    in
    match meth with
    | #Methods.support_method as meth ->
        Servant.reroot svt >>= fun () ->
        release_svt () >>= fun state ->
	no_cont (support pool client state meth)
    | #Methods.bremer_support as meth ->
        Servant.reroot svt >>= fun () ->
        release_svt () >>= fun state ->
	no_cont (bremer pool client state meth)
    | `OnEachTree (todo, combine) ->
        release_svt () >>= fun state ->
	no_cont (on_each_tree pool client state todo combine)
    | `ParallelPipeline (times, todo, composer, continue) ->
        release_svt () >>= fun state ->
	parallel_pipeline pool client state times todo composer >>= fun s ->
	return (s, continue)
