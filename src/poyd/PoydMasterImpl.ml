open PoydPrelude
open PoydDefs

module L = (val FundLog.make "PoydMasterImpl" : FundLog.S)

module Pool = PoydWorkerPool


module Client = PoydClientStub
module Servant = PoydServantStub


type t = {
    pool : Pool.t;
    checkpoint_secs : float;
}

let create ~checkpoint_secs = {
    pool = Pool.create ();
    checkpoint_secs = checkpoint_secs;
}


let register_servant m s =
    Pool.put m.pool s >>= fun () ->
    Servant.get_name s >>= fun name ->
    L.info "Registered servant %s to pool" name


type task = {
    id : int;
    mutable state : PoydState.t;
    mutable client : Client.t;
    mutex : Lwt_mutex.t;
}


exception Restart of PoydState.t * script list

        
        
let run_task master task cmds =
    let run_tick servant cmds  =
        Servant.get_name servant >>= fun s_name ->
        let tick = Lwt_unix.sleep master.checkpoint_secs in
        let rec loop = function
            | [] -> 
                Servant.final_report servant >>= fun () ->
                return None
            | `ParallelPipeline (times, todo, composer, continue) :: rest -> 
                begin
                    L.trace (fun () -> PoydState.receive servant)
                        "Receive state from servant" >>= fun state ->
                    L.info "Releasing servant before ParallelPipeline"
                    >>= fun () ->
                    Pool.put master.pool servant >>= fun () ->
                    PoydParallel.parallel_pipeline master.pool task.client
                        state times todo composer >>= fun new_state ->
                    (* This looks bad, but this is the simplest way to
                       tell the top loop to get a new servant. *)
                    fail (Restart (new_state, continue @ rest))
                end
            | `OnEachTree (todo, composer) :: rest -> begin
                L.trace (fun () -> PoydState.receive servant)
                    "Receive state from servant" >>= fun state ->
                L.info "Releasing servant before OnEachTree"
                >>= fun () ->
                Pool.put master.pool servant >>= fun () ->
                PoydParallel.on_each_tree master.pool task.client
                    state todo composer >>= fun new_state ->
                fail (Restart (new_state, rest))
            end
            | cmd :: rest ->
                L.trace 
                    (fun () -> Servant.begin_script servant [cmd])
                    "execute_script: %s"
                    (Analyzer.script_to_string cmd) >>= fun () ->
                match state tick with
                | Sleep -> loop rest
                | _ -> 
                    PoydState.receive servant >>= fun state ->
                    L.info "Task %d: Saved checkpoint from %s" task.id s_name 
                    >>= fun () ->
                    Client.output_status task.client Status.Information
                        "Saved checkpoint" >>= fun () ->
                    return (Some (state, rest))
        in 
        loop cmds 
    in
    let rec run_on_servant servant restart_state cmds =
        try_bind 
            (fun () -> run_tick servant cmds)
            (function
              | None -> 
                  return ()
              | Some (state2, cmds2) -> 
                  run_on_servant servant state2 cmds2)
            (function
              | Fund.ConnectionError _ 
              | Fund.RouteError ->
                  Servant.get_name servant >>= fun s_name ->
                  L.info "Task %d: Error connecting to servant %s" 
                      task.id s_name >>= fun () ->
                  fail (Restart (restart_state, cmds))
              | exn -> fail exn)
    in
    let rec run_cmds state cmds =
        L.info "Task %d: Requesting servant" task.id >>= fun () ->
        Pool.get master.pool 0 >>= fun servant ->
        Servant.get_name servant >>= fun s_name ->
        L.info "Task %d: Allocated servant %s" task.id s_name >>= fun () ->
        catch (fun () ->
            Servant.set_client servant task.client >>= fun () ->
            Servant.clear_output servant >>= fun () ->
            PoydState.send state servant >>= fun () ->
            result (fun () -> run_on_servant servant state cmds) >>= function
            | Bad (Fund.ConnectionError _ | Fund.RouteError | Restart _ as exn) ->
                fail exn
            | res ->
                PoydState.receive servant >>= fun new_state ->
                Servant.get_output servant >>= fun output ->
                task.state <- new_state;
                Client.execute_output task.client output >>= fun () ->
                Pool.put master.pool servant >>= fun () ->
                L.info "Task %d: Released servant %s to pool" task.id s_name
                >>= fun () ->
                run_result res)
            (function
              | Restart (rstate, rcmds) ->
                  L.info "Task %d: Restarting from checkpoint" task.id >>= fun () ->
                  Client.output_status task.client Status.Information
                      "Restarting from checkpoint" >>= fun () ->
                  run_cmds rstate rcmds
              | Fund.ConnectionError _
              | Fund.RouteError ->
                  L.info "Task %d: Error connecting to servant %s" task.id s_name >>= fun () ->
                  run_cmds state cmds
              | exn ->
                  fail exn)
    in
    L.info "Task %d: Running commands" task.id >>= fun () ->
    run_cmds task.state cmds >>= fun () ->
    L.info "Task %d: Commands finished" task.id


let next_task_id = ref 0

let create_task master client =
    let task = {
        id = (incr next_task_id; !next_task_id);
        state = PoydState.create ();
        client = client;
        mutex = Lwt_mutex.create ();
    } in
    let f cmds = 
        Lwt_mutex.with_lock task.mutex
            (fun () -> run_task master task cmds)
    in
    let h = Fund.publish f 
    in
    detach (fun () ->
        finalize (fun () -> Client.wait_finish client)
            (fun () -> 
                L.info "Task %d: Finished" task.id >>= fun () ->
                Fund.withdraw h; return ()));
    Client.get_name task.client >>= fun c_name ->
    L.info "Task %d: Created for client %s" task.id c_name >>= fun () ->
    return h
        
        

