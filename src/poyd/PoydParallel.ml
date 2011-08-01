open PoydDefs


module Servant = PoydServantStub
module Pool = PoydWorkerPool

module L = (val FundLog.make "PoydParallel" : FundLog.S)

module E = BatEnum

(* Parallel stuff *)

module Task = struct
    type t = 
        | Initial of PoyRandom.t
        | Combine of int * tree Sexpr.t lwt (* Or something more compact? *)

    let cost = function
        | Initial _ -> 1
        | Combine (sz, _) -> sz
            
    let compare a b = compare (cost a) (cost b)
end

module Tasks = struct
    include BatHeap.Make(Task)
    let cost t = 
        E.fold (+) 0 (E.map Task.cost (enum t))
    let create state n =
        PoyRandom.with_state state.PoydState.rng (fun () ->
            of_enum
                (E.take n
                     (E.from (fun () -> 
                         Task.Initial (PoyRandom.fork ())))))
    let view t =
        if size t = 0
        then None
        else Some (find_min t, del_min t)

end


let gensym_count = ref (-1)
let gensym () =
    incr gensym_count;
    Printf.sprintf "__poy_id_%d" !gensym_count


type result =
  | WorkerReady of Servant.t * Tasks.t
  | JobFailed of Tasks.t
  | NewWorker of Servant.t
  | Nop

let catch_task lost_tasks thunk =
    catch thunk
        (fun exn -> return (JobFailed lost_tasks)) 

let parallel_pipeline pool client state n todo combine =
    let tmp_id = gensym ()
    in
    let build_script = `Set ([`Data], tmp_id) :: todo @ combine 
    in
    let start_job (w : Servant.t) (task : Task.t) (done_tasks : Tasks.t) 
            : result lwt lwt
            = return (
            catch_task done_tasks (fun () ->
                match task with
                | Task.Initial rng -> begin
                    L.info "Begin Initial, rng %08x" (Hashtbl.hash rng) >>= fun () ->
                    Servant.set_rng w rng >>= fun () ->
                    Servant.begin_script w build_script >>= fun () ->
                    return (WorkerReady(w, Tasks.add task done_tasks))
                end
                | Task.Combine(sz, trees_t) -> begin
                    L.info "Begin Combine, waiting for trees" >>= fun () ->
                    try_bind (fun () -> trees_t)
                        (fun trees ->
                            L.info "Got trees, sending" >>= fun () ->
                            Servant.add_stored_trees w trees >>= fun () ->
                            L.info "Trees sent, begin script" >>= fun () ->
                            Servant.begin_script w combine >>= fun () ->
                            L.info "Script done" >>= fun () ->
                            return (WorkerReady(w, Tasks.add task done_tasks)))
                        (fun exn ->
                            L.info "Couldn't receive trees for merging" >>= fun () ->
                            return (WorkerReady(w, done_tasks)))
                end))
    in
    let process_event (running, pending) (event : result) = match event with
        | WorkerReady(w, done_tasks) -> begin
            L.dbg "WorkerReady" >>= fun () ->
            match Tasks.view pending, Tasks.view done_tasks with
            | Some (Task.Combine _ as task, rest), Some _
            | Some (Task.Initial _ as task, rest), _ -> begin
                start_job w task done_tasks >>= fun job ->
                return (job :: running, rest)
            end
            | Some (Task.Combine _, _), None
            | None, _ -> begin
                if Tasks.size done_tasks = 0 
                then (Pool.put pool w >>= fun () ->
                      return (running, pending))
                else let sz = Tasks.cost done_tasks in
                     let trees_t = apply Servant.get_stored_trees w in
                     let job = catch_task done_tasks (fun () -> 
                         trees_t >>= fun _ ->
                         L.dbg "Trees received, freeing servant" >>= fun () ->
                         Pool.put pool w >>= fun () ->
                         return Nop) in
                     return (job :: running, 
                             Tasks.add (Task.Combine(sz, trees_t)) pending)
            end
        end
        | JobFailed lost_tasks ->
            L.dbg "JobFailed" >>= fun () ->
            return (running, Tasks.merge lost_tasks pending)
        | Nop ->
            L.dbg "Nop" >>= fun () ->
            return (running, pending)
        | NewWorker w ->
            L.dbg "NewWorker" >>= fun () ->
            Servant.set_client w client >>= fun () ->
            PoydState.send state w >>= fun () ->
            Servant.begin_script w [`Store ([`Data], tmp_id)] >>= fun () ->
            let job = return (WorkerReady (w, Tasks.empty)) in
            return (job :: running, pending)
    in                
    let get_worker pri =
        try_bind 
            (fun () ->
                L.dbg "requesting new worker from pool" >>= fun () ->
                Pool.get pool pri)
            (fun w ->
                L.dbg "worker received" >>= fun () ->
                return (NewWorker w))
            (function
              | Canceled ->
                  L.dbg "worker request canceled" >>= fun () ->
                  return Nop
              | exn -> fail exn)
    in
    let rec loop (running, pending) = 
        L.dbg "%d running jobs, %d pending tasks" 
            (List.length running) (Tasks.size pending) >>= fun () ->
        match running, Tasks.view pending with
        | [], Some (Task.Combine (_, trees_t), rest) when Tasks.size rest = 0 ->
            trees_t
        | _ ->
            (* If there are jobs that could use extra workers, request one. *)
            let worker_req_t = match Tasks.view pending with
                (* Priority is just the number of workers we already have. *)
                | Some (Task.Initial _, _) -> get_worker (List.length running)
                | _ -> halt ()
            in
            nchoose_split (worker_req_t :: running) >>= fun (events, running1) ->
            (* If we got a worker, this does nothing. Otherwise the getter
               returns a `Nop and is discarded at the next loop iteration. *)
            cancel worker_req_t;
            let running2 = BatList.remove_if ((==) worker_req_t) running1 in
            Lwt_list.fold_left_s process_event (running2, pending) events 
            >>= loop
    in
    L.info "Begin parallel, rng now %08x" (Hashtbl.hash state.PoydState.rng)
    >>= fun () ->
    loop ([], Tasks.create state n) >>= fun trees ->
    L.info "End parallel, rng now %08x" (Hashtbl.hash state.PoydState.rng) 
    >>= fun () ->
    return { state with PoydState.run = { 
        state.PoydState.run with stored_trees = trees 
    } }
    
    
