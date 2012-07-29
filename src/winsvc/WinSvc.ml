open Lwt

type status_handle

type accept = 
    | AStop
    | APauseContinue
    | AShutdown
    | APreShutdown
    | AUnknown

type control = 
    | CStop 
    | CPause
    | CContinue
    | CInterrogate
    | CShutdown
    | CPreShutdown
    | CParamChange
    | CUnknown

type state =
    | SStopped
    | SStartPending
    | SStopPending
    | SRunning
    | SContinuePending
    | SPausePending
    | SPaused

type status = {
    current_state : state;
    controls_accepted : accept array;
    exit_code : int;
}

type version = {
    major : int;
    minor : int;
    build : int;
    csd : string;
}

external start_dispatcher : string -> bool 
    = "caml_winsvc_start_dispatcher"
external register_handler : string -> status_handle
    = "caml_winsvc_register_handler"
external set_status : status_handle -> status -> unit
    = "caml_winsvc_set_status"
external get_version : unit -> version
    = "caml_win32_get_version"

let version = get_version ()

let have_preshutdown = version.major >= 6

(* BEGIN snippet from lwt 2.4.0 *)

type 'a result =
  | Value of 'a
  | Error of exn

(* Queue of [unit -> unit Lwt.t] functions. *)
let jobs = Queue.create ()

(* Mutex to protect access to [jobs]. *)
let jobs_mutex = Mutex.create ()

let job_notification =
  Lwt_unix.make_notification
    (fun () ->
       (* Take the first job. The queue is never empty at this
          point. *)
       Mutex.lock jobs_mutex;
       let thunk = Queue.take jobs in
       Mutex.unlock jobs_mutex;
       ignore (thunk ()))

let run_in_main f =
  let channel = Event.new_channel () in
  (* Create the job. *)
  let job () =
    (* Execute [f] and wait for its result. *)
      try_bind f
          (fun ret -> return (Value ret))
          (fun exn -> return (Error exn)) >>= fun result ->
    (* Send the result. *)
      Event.sync (Event.send channel result);
      return ()
  in
  (* Add the job to the queue. *)
  Mutex.lock jobs_mutex;
  Queue.add job jobs;
  Mutex.unlock jobs_mutex;
  (* Notify the main thread. *)
  Lwt_unix.send_notification job_notification;
  (* Wait for the result. *)
  match Event.sync (Event.receive channel) with
    | Value ret -> ret
    | Error exn -> raise exn

(* END snippet *)


module Future = struct
    type 'a t = {
        mutex : Mutex.t;
        mutable value : 'a option;
    }
    let create () = {
        mutex = Mutex.create ();
        value = None;
    }
    let get f = match f.value with
        | None -> failwith "Uninitialized future"
        | Some v -> v
    let set f v = 
        Mutex.lock f.mutex;
        let succ = match f.value with
            | None -> f.value <- Some v; true
            | Some _ -> false
        in
        Mutex.unlock f.mutex;
        if not succ then failwith "Future reinitialized"
end

type service = {
    name : string;
    handle : status_handle Future.t;
    main : string array -> unit Lwt.t;
    stop : (bool -> unit Lwt.t) option;
    pause : (bool -> unit Lwt.t) option;
}


let the_service = Future.create ()

let svc_accept_flags svc = 
    let check_flag = function
        | None, _ -> []
        | Some _, codes -> codes
    in
    List.(concat (map check_flag [
        svc.stop, [AStop; AShutdown];
        svc.stop, if have_preshutdown then [APreShutdown] else [];
        svc.pause, [APauseContinue];
    ]))

let svc_set_state ?(exitcode=0) svc state accept_controls =
    set_status (Future.get svc.handle) {
        current_state = state;
        controls_accepted = Array.of_list (
            if accept_controls then svc_accept_flags svc else []);
        exit_code = exitcode 
    }

let service_main argv = 
    let svc = Future.get the_service
    in
    Printf.eprintf "Register handler\n%!";
    Future.set svc.handle (register_handler svc.name);
    Printf.eprintf "Set status to running\n%!";
    svc_set_state svc SRunning true;
    Printf.eprintf "Call run handler\n%!";
    let exitcode = 
        try
            run_in_main (fun () -> svc.main argv);
            0
        with
        | exn -> Printf.eprintf "Got exception from run handler: %s\n%!" 
            (Printexc.to_string exn);
            1066
    in
    Printf.eprintf "Set status to stopped\n%!";
    svc_set_state ~exitcode svc SStopped false

let _ = Callback.register "WinSvc.service_main" service_main
    
let ctrl_handler ctrl = 
    let svc = Future.get the_service
    in
    let bkt acc pre hdl arg post =
        (match hdl with 
        | None -> fail (Failure "unexpected control message")
        | Some h -> return h) >>= fun h ->
        svc_set_state svc pre acc;
        h arg >>= fun () ->
        svc_set_state svc post acc;
        return ()
    in
    run_in_main (fun () -> match ctrl with
    | CStop -> bkt false SStopPending svc.stop false SStopPending
    | CShutdown -> bkt false SStopPending svc.stop true SStopPending
    | CPreShutdown -> bkt false SStopPending svc.stop true SStopPending
    | CPause -> bkt true SPausePending svc.pause false SPaused
    | CContinue -> bkt true SContinuePending svc.pause true SRunning
    | _ -> fail (Failure "Unknown control!"))
    
let _ = Callback.register "WinSvc.ctrl_handler" ctrl_handler

let run ~name ~main ?stop ?pause () =
    let svc = { name; handle = Future.create (); main; stop; pause; }
    in
    Future.set the_service svc;
    Printf.eprintf "calling start_dispatcher in new thread\n%!";
    Lwt_preemptive.detach (fun () ->
        Printf.eprintf "new thread, now really calling dispatcher\n%!";
        let ret = start_dispatcher name
        in
        Printf.eprintf "dispatcher finished\n%!";
        ret) () >>= function
      | false -> fail (Failure "couldn't start dispatcher")
      | true -> return ()
