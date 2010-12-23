open NcPrelude

module Mgr = NcTaskMgr

type rqid = int

type domain = Uuidm.t

module SChan = NcSyncChannel


let section = Lwt_log.Section.make "NcConnection"

let dbg fmt = 
    Lwt_log.debug_f ~section fmt

let pprint printer () v =
    let out = BatIO.output_string () in
    let outc = BatIO.cast_output out in
    printer false outc v;
    BatIO.close_out out


type request = [
  | `IAm of domain
  | `Apply of (obj, obj) NcLocalHandle.t * obj
  | `Ping
  | `Cancel of rqid
  | `Root of string
  | `Close
]

let request_printer paren out = 
    let open BatIO in
	function
	  | `Close -> nwrite out "`Close"
	  | `IAm d -> printf out "`IAm \"%s\"" (Uuidm.to_string d)
	  | `Apply (h, v) -> 
	      printf out "`Apply (_, _)"
	  | `Ping -> nwrite out "`Ping"
	  | `Root rid -> printf out "`Root %a"
	      (BatString.t_printer true) rid
	  | `Cancel rqid -> printf out "`Cancel %d" rqid

type response = [
  | `OK of obj
  | `Error of exn
]

let response_printer paren out =
    let open BatIO in
	function 
	  | `OK v -> nwrite out "`OK _"
	  | `Error _ -> nwrite out "`Error"

type message =
    | Request of rqid * request
    | Response of rqid * response

let message_printer paren out =
    let open BatIO in
	function
	  | Request (rqid, rq) ->
	      printf out "Request (%d, %a)" 
		  rqid
		  (request_printer false) rq
	  | Response (rqid, resp) ->
	      printf out "Response (%d, %a)"
		  rqid
		  (response_printer false) resp

type t = {
    in_ch : IO.input_channel;
    out_ch : IO.output_channel;
    mutable remote_domain : domain option;
    mutable next_rqid : rqid;
    pending_remote : (rqid, response mvar) Hashtbl.t;
    pending_local : (rqid, response lwt) Hashtbl.t;
    local_mgr : Mgr.t;
    remote_mgr : Mgr.t;
}

let make (in_ch, out_ch) = {
    in_ch;
    out_ch;
    remote_domain = None;
    next_rqid = 1;
    pending_remote = Hashtbl.create 7;
    pending_local = Hashtbl.create 7;
    local_mgr = Mgr.create ();
    remote_mgr = Mgr.create ();
}

let local_domain : domain =
    Uuidm.create `V4

module W = Weaktbl.MakeDef(struct type t = domain end)

let domain_table : t W.t = 
    W.create 17

let local_roots = Hashtbl.create 7

let write_msg conn (msg : message) =
    dbg "write_msg: %a" (pprint message_printer) msg >>= fun () ->
    IO.write_value conn.out_ch msg

let read_msg conn : message lwt =
    IO.read_value conn.in_ch >>= fun msg ->
    dbg "read_msg: %a" (pprint message_printer) msg;
    return msg
	

let domain conn = conn.remote_domain

exception Error

let do_request conn rq = 
    let rqid = conn.next_rqid in
    conn.next_rqid <- rqid + 1;
    let mv = MVar.create_empty () in
    Hashtbl.add conn.pending_remote rqid mv;
    finalize (fun () ->
	dbg "-> %d Request" rqid >>= fun () ->
	write_msg conn (Request (rqid, rq)) >>= fun () ->
	MVar.take mv >>= function
	  | `OK r -> 
	      dbg "<- %d OK" rqid >>= fun () ->
	      return r
	  | `Error exn -> 
	      dbg "<- %d Error" rqid >>= fun () ->
	      NcExn.immigrate exn;
	      fail exn)
	(fun () ->
	    return (Hashtbl.remove conn.pending_remote rqid))

let request conn rq =
    Mgr.task conn.local_mgr (fun () -> do_request conn rq)

let close conn =
    detach (fun () -> request conn `Close);
    Mgr.close conn.local_mgr

let shutdown conn =
    IO.close conn.in_ch >>= fun () ->
    IO.close conn.out_ch

let ok = `OK (Obj.repr ())

let handle_request conn rq = function
    | `Close -> 
	detach (fun () -> 
	    (close conn
	     <&> Mgr.close conn.local_mgr) >>= fun () ->
	    shutdown conn);
	return ok
    | `IAm remote_domain -> begin 
	match conn.remote_domain with
	| None -> 
	    conn.remote_domain <- Some remote_domain;
	    W.set domain_table remote_domain conn;
	    return ok
	| Some _ ->
	    return `Error
    end
    | `Apply (h, v) ->
	NcLocalHandle.apply h v >>= fun r ->
	return (`OK r)
    | `Ping -> 
	return ok
    | `Root rid -> begin
	match Hashtbl.find_option local_roots rid with
	| Some o -> return (`OK o)
	| None -> return `Error
    end
    | `Cancel rqid -> 
	let t = Hashtbl.find conn.pending_local rqid in
	match state t with
	| Sleep -> begin
	    cancel t;
	    match state t with
	    | Fail Canceled -> return ok
	    | _ -> return `Error
	end
	| _ -> return `Error
	    
let handle_message conn = function
    | Response (rqid, resp) ->
	let p = conn.pending_remote in
	if Hashtbl.mem p rqid then 
	    let mv = Hashtbl.find p rqid in
	    MVar.put mv resp
	else
	    return ()
    | Request (rqid, rq) ->
	Mgr.task conn.remote_mgr (fun () ->
	    let t = handle_request conn rqid rq in
	    Hashtbl.add conn.pending_local rqid t;
	    finalize (const t)
		(fun () -> 
		    return (Hashtbl.remove conn.pending_local rqid))
	    >>= fun resp ->
	    write_msg conn (Response (rqid, resp)))

let rec read_loop conn =
    read_msg conn >>= fun msg ->
    detach (fun () -> 
	dbg "handling: %a" (pprint message_printer) msg >>= fun () ->
	handle_message conn msg);
    read_loop conn


let create chans =
    let conn = make chans in
    detach (fun () -> read_loop conn);
    detach (fun () -> request conn (`IAm local_domain));
    conn

let connect addr = 
    IO.open_connection addr >>= fun chans ->
    return (create chans)

type listener = IO.server

let create_listener addr = 
    IO.establish_server addr
	(fun chans ->
	    ignore (create chans))

let apply conn h v =
    request conn (`Apply (Obj.magic h, Obj.repr v)) >>= fun o ->
    return (Obj.obj o)

let get_root conn name =
    request conn (`Root name) >>= fun o ->
    return (Obj.obj o)

let set_root (name : string) v =
    Hashtbl.add local_roots name (Obj.repr v)

let lookup domain =
    W.get domain_table domain






