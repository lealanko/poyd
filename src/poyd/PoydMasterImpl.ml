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


(* Distinguish client-side connection errors from 
   servant by wrapping them in ClientExn. *)

let client_call thunk =
    catch thunk
        (fun exn -> raise (ClientExn exn))
        
let run_task master task cmds =
    let module L = 
            (val FundLog.make (Printf.sprintf "PoydMasterImpl:%d" task.id)
                    : FundLog.S)
    in
    let commit_output servant =
        Servant.get_output servant >>= fun output ->
        client_call (fun () ->
            Client.execute_output task.client output)
    in
    let run_tick servant cmds  =
        Servant.get_name servant >>= fun s_name ->
        let tick = Lwt_unix.sleep master.checkpoint_secs in
        let rec loop = function
            | [] -> 
                Servant.final_report servant >>= fun () ->
                commit_output servant >>= fun () ->
                return None
	    | (#par_method as meth) :: rest -> begin
                L.trace_ (fun () ->
                    PoydParallel.run_parallel master.pool task.client
                        servant meth)
                    "PoydParallel: %s" (PoydUtil.script_txt meth) 
                >>= fun (new_state, cont) ->
                (* This looks bad, but this is the simplest way to
                   tell the top loop to get a new servant. *)
                L.dbg "Raise Restart" >>= fun () ->
                fail (Restart (new_state, cont @ rest))
            end
            | cmd :: rest ->
                L.trace 
                    (fun () -> Servant.begin_script servant [cmd])
                    "execute_script: %s"
                    (PoydUtil.script_txt cmd) >>= fun () ->
                match state tick with
                | Sleep -> loop rest
                | _ -> 
                    PoydState.receive servant >>= fun state ->
                    commit_output servant >>= fun () ->
                    L.info "Saved checkpoint from %s" s_name 
                    >>= fun () ->
                    client_call (fun () ->
                        Client.output_status task.client Status.Information
                            "Saved checkpoint") >>= fun () ->
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
            (fun exn ->
                L.dbg "Caught %s" (Printexc.to_string exn) >>= fun () ->
                match exn with
                | Abort -> begin
                    catch (fun () ->
                        Servant.set_client servant None)
                        (fun exn -> return ()) >>= fun () ->
                    Servant.get_name servant >>= fun s_name ->
                    L.info "Servant %s was aborted" s_name 
                    >>= fun () ->
                    fail (Restart (restart_state, cmds))
                end
                | Fund.ConnectionError _ 
                | Fund.RouteError -> begin
                    Servant.get_name servant >>= fun s_name ->
                    L.error "Error connecting to servant %s" s_name
                    >>= fun () ->
                    fail (Restart (restart_state, cmds))
                end
                | exn -> 
                    (* It was an application level exception, both the client and 
                       servant are alive so we can output pending data *)
                    commit_output servant >>= fun () ->
                    fail exn)
    in
    let rec run_cmds state cmds =
        L.dbg "Requesting servant" >>= fun () ->
        Pool.get master.pool 0 >>= fun servant ->
        Servant.get_name servant >>= fun s_name ->
        L.dbg "Allocated servant %s" s_name >>= fun () ->
        catch (fun () ->
            Servant.set_client servant (Some task.client) >>= fun () ->
            Servant.clear_output servant >>= fun () ->
            PoydState.send state servant >>= fun () ->
            result (fun () -> run_on_servant servant state cmds) >>= function
            | Bad (Fund.ConnectionError _ | Fund.RouteError | Restart _ as exn) ->
                (* Don't return the servant to pool, it failed. *)
                fail exn
            | Ok ret -> begin
                PoydState.receive servant >>= fun new_state ->
                Servant.get_output servant >>= fun output ->
                task.state <- new_state;
                Servant.set_client servant None >>= fun () ->
                Pool.put master.pool servant >>= fun () ->
                L.dbg "Released servant %s to pool" s_name >>= fun () ->
                client_call (fun () ->
                    Client.execute_output task.client output) >>= fun () ->
                return ret
            end
            | Bad exn -> 
                Servant.set_client servant None >>= fun () ->
                Pool.put master.pool servant >>= fun () ->
                L.dbg "Released servant %s to pool" s_name >>= fun () ->
                fail exn)
            (function
              | Restart (rstate, rcmds) ->
                  L.dbg "Restarting from checkpoint" >>= fun () ->
                  run_cmds rstate rcmds
              | Fund.ConnectionError _
              | Fund.RouteError ->
                  L.error "Error connecting to servant %s" s_name 
                  >>= fun () ->
                  run_cmds state cmds
              | ClientExn (exn) 
              | exn ->
                  fail exn)
    in
    L.dbg "Running commands" >>= fun () ->
    run_cmds task.state cmds >>= fun () ->
    L.dbg "Commands finished"

let run_task master task cmds =
    L.trace_ (fun () -> run_task master task cmds) "run_task _ %d _" task.id

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
        
        

