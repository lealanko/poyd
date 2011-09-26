open PoydDefs


module Servant = PoydServantStub
module Pool = PoydWorkerPool

module L = (val FundLog.make "PoydParallel" : FundLog.S)

module E = BatEnum

module Q = struct
    include Queue
    include BatQueue
    include BatQueue.Exceptionless
end

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

type ('a, 'b) state = {
    to_generate : 'a Q.t;
    to_combine : 'b lwt Q.t;
}


type ('a, 'b) worker = {
    servant : Servant.t;
    generated : 'a Q.t;
    combined : 'b Q.t;
}

let rec do_worker generate combine receive_result w st =
    let loop1 () =
        match Q.take st.to_generate, combine with
        | Some task, _ -> begin 
            Q.push w.generated task;
            generate w task >>= fun () ->
            return true
        end
        | None, Some f ->
            f w st.to_combine
        | None, None -> return false
    in
    let rec loop2 () =
        loop1 () >>= function
          | true ->
              loop2 ()
          | false ->
              let receive_t = apply receive_result w 
              in
              Q.push st.results receive_t;
              receive_t >>= fun _ ->
              return ()
    in
    try_bind loop2
        (fun ret -> return (Ok ret))
        (fun exn ->
            Q.transfer w.generated st.to_generate;
            Q.iter (fun b -> Q.push (return b) st.to_combine)
                w.combined;
            return (Bad ret))

let create_worker servant = {
    servant = servant;
    generated = Queue.create ();
    combined = Queue.create ();
}


let worker_requester ... =
    let n_workers = List.length st.active_workers 
    in
    let rq_t = 
        if Q.length st.to_generate > 0 || 
            (Q.length st.to_combine > 1 && n_workers = 0)
        then apply Pool.get pool n_workers
        else halt ()
    in 
    rq_t <?> join (map (fun w -> w.thr) st.active_workers) >>= fun () ->
    cancel rq_t;
    match state rq_t with
    | Return servant ->
        let new_w = create_worker servant () 
        in
        st.active_workers <- new_w :: st.active_workers;
        worker_requester ...
    | Fail _ ->
        
                
    
        
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
        
    

let parallel_pipeline pool client state n todo combine =
    let tmp_id = gensym ()
    in
    let build_script = `Set ([`Data], tmp_id) :: todo @ combine 
    in
    let generate w rng =
        Servant.set_rng w.servant rng >>= fun () ->
        Servant.begin_script w.servant build_script
    in
    let combine w to_combine = match Q.length to_combine with
        | 0 -> 
        | 1 when Q.is_empty w.generated && Q.is_empty w.combined ->
            return false
        | _ ->
            try_bind (fun () -> 
                Q.pop to_combine >>= fun t1 ->
                if Q.is_empty w.generated && Q.is_empty w.combined 
                then (Q.pop to_combine >>= fun t2 ->
                      return (t1, Some t2))
                else return (t1, None))
                (fun (t1, st2) ->
                    Servant.add_stored_trees w t1 >>= fun () ->
                    (match st2 with
                    | None -> return ()
                    | Some t2 -> Servant.add_stored_trees w t2) >>= fun () ->
                    Servant.begin_script w combine >>= fun () ->
                    return true)
                (fun exn -> 
                    (* tree reception failed, but this worker is still ok *)
                    return false)
    in
    


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
    
    
