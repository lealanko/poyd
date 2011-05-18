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
}

let create () = {
    registry = Registry.create ();
}

type task = {
    master : t;
    client : Client.t;
    
}



let backup_interval_seconds = 1.0


let register_servant m s =
    Servant.get_name s >>= fun name ->
    L.info "REGISTER %s" name >>= fun () ->
    Registry.add m.registry s


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

exception Restart of task_state * script list

let run_task master client cmds =
    Client.get_name client >>= fun c_name ->
    L.info "BEGIN TASK from %s" c_name >>= fun () ->
    let run_tick servant cmds  =
        Servant.get_name servant >>= fun s_name ->
        let tick = Lwt_unix.sleep backup_interval_seconds in
        let rec loop = function
            | [] -> return None
            | cmd :: rest ->
                L.trace 
                    (fun () -> Servant.execute_script servant [cmd])
                    "execute_script: %s"
                    (Analyzer.script_to_string cmd) >>= fun () ->
                match state tick with
                | Sleep -> loop rest
                | _ -> 
                    L.info "CHECKPOINT %s" s_name >>= fun () ->
                    get_state servant >>= fun state ->
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
                  L.info "ERROR at %s" s_name >>= fun () ->
                  fail (Restart (restart_state, cmds))
              | exn -> fail exn)
    in
    let rec run_cmds state cmds =
        Registry.get master.registry >>= fun servant ->
        Servant.get_name servant >>= fun s_name ->
        L.info "ALLOCATE %s FOR %s" s_name c_name >>= fun () ->
        try_bind 
            (fun () ->
                Servant.set_client servant client >>= fun () ->
                set_state servant state >>= fun () ->
                run_on_servant servant state cmds)
            (fun () -> 
                L.info "RELEASE %s FROM %s" s_name c_name >>= fun () ->
                Registry.add master.registry servant)
            (function
              | Restart (rstate, rcmds) ->
                  L.info "RESTART %s" c_name >>= fun () ->
                  run_cmds rstate rcmds
              | Fund.ConnectionError _
              | Fund.RouteError ->
                  L.info "ERROR at %s" s_name >>= fun () ->
                  run_cmds state cmds
              | exn ->
                (* A non-Fund exception simply means that an exception was raised
                   in the the code in the servant, but the servant is still okay so we 
                   put it back to the registry for reuse. *)
                  L.info "RELEASE %s FROM %s" s_name c_name >>= fun () ->
                  Registry.add master.registry servant >>= fun () ->
                  fail exn)
    in
    let state = new_state () in
    run_cmds state cmds >>= fun () ->
    L.info "END TASK from %s" c_name

