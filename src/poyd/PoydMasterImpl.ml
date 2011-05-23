open PoydPrelude
open PoydDefs

module L = (val FundLog.make "PoydMasterImpl" : FundLog.S)


module Client = PoydClientStub
module Servant = PoydServantStub

module Registry = struct
    type t = {
        cond : unit Lwt_condition.t;
        mutable set : Servant.t BatSet.t;
    }

    let create () = { 
        cond = Lwt_condition.create ();
        set = BatSet.empty;
    }

    let add reg srv = 
        reg.set <- BatSet.add srv reg.set;
        Lwt_condition.broadcast reg.cond ();
        return ()

    let rec get reg =
        try
            let srv, rest = BatSet.pop reg.set in
            reg.set <- rest;
            return srv
        with Not_found ->
            Lwt_condition.wait reg.cond >>= fun () ->
            get reg

    let loan reg fn =
        get reg >>= fun srv ->
        result (fun () -> fn srv) >>= fun r ->
        (match r with
        | Bad (Fund.RouteError | Fund.ConnectionError _) -> return ()
        | _ -> add reg srv) >>= fun () ->
        run_result r
            
end

type t = {
    registry : Registry.t;
    checkpoint_secs : float;
}

let create ~checkpoint_secs = {
    registry = Registry.create ();
    checkpoint_secs = checkpoint_secs;
}


let register_servant m s =
    Registry.add m.registry s >>= fun () ->
    Servant.get_name s >>= fun name ->
    L.info "Registered servant %s to pool" name


type task_state = {
    run : Phylo.r;
    rng : PoyRandom.t;
}

let new_state () = {
    run = Phylo.empty ();
    rng = Random.State.make [| truncate (Unix.time ()) |];
}

let get_state servant =
    Servant.get_run servant >>= fun run ->
    Servant.get_rng servant >>= fun rng ->
    return { run; rng }

let set_state servant state = 
    Servant.set_run servant state.run >>= fun () ->
    Servant.set_rng servant state.rng

type task = {
    id : int;
    mutable state : task_state;
    mutable client : Client.t;
    mutex : Lwt_mutex.t;
}

exception Restart of task_state * script list

let run_task master task cmds =
    let run_tick servant cmds  =
        Servant.get_name servant >>= fun s_name ->
        let tick = Lwt_unix.sleep master.checkpoint_secs in
        let rec loop = function
            | [] -> 
                Servant.final_report servant >>= fun () ->
                return None
            | cmd :: rest ->
                L.trace 
                    (fun () -> Servant.execute_script servant [cmd])
                    "execute_script: %s"
                    (Analyzer.script_to_string cmd) >>= fun () ->
                match state tick with
                | Sleep -> loop rest
                | _ -> 
                    get_state servant >>= fun state ->
                    L.info "Task %d: Saved checkpoint from %s" task.id s_name >>= fun () ->
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
        Registry.get master.registry >>= fun servant ->
        Servant.get_name servant >>= fun s_name ->
        L.info "Task %d: Allocated servant %s" task.id s_name >>= fun () ->
        try_bind 
            (fun () ->
                Servant.set_client servant task.client >>= fun () ->
                set_state servant state >>= fun () ->
                run_on_servant servant state cmds)
            (fun () -> 
                get_state servant >>= fun state ->
                task.state <- state;
                Registry.add master.registry servant >>= fun () ->
                L.info "Task %d: Released servant %s to pool" task.id s_name)
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
                (* A non-Fund exception simply means that an exception was raised
                   in the the code in the servant, but the servant is still okay so we 
                   put it back to the registry for reuse. *)
                  (match exn with
                  | MainUtil.ExitPoy r ->
                      (* Let's not do a useless checkpoint if we know we
                         won't use the state for anything any more. *)
                      return () 
                  | _  ->
                      get_state servant >>= fun state ->
                      task.state <- state;
                      return ()) >>= fun () ->
                  Registry.add master.registry servant >>= fun () ->
                  L.info "Task %d: Released servant %s to pool" task.id s_name >>= fun () ->
                  fail exn)
    in
    L.info "Task %d: Running commands" task.id >>= fun () ->
    run_cmds task.state cmds >>= fun () ->
    L.info "Task %d: Commands finished" task.id


let next_task_id = ref 0

let create_task master client =
    let task = {
        id = (incr next_task_id; !next_task_id);
        state = new_state ();
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
        
        

