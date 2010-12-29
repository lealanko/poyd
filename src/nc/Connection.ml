open NcPrelude

include ConnectionDefs

module Mgr = NcTaskMgr

type rqid = int

let section = Lwt_log.Section.make "NcConnection"

let dbg fmt = 
    Lwt_log.debug_f ~section fmt

let pprint printer () v =
    let out = BatIO.output_string () in
    let outc = BatIO.cast_output out in
    printer false outc v;
    BatIO.close_out out

module type APPLY = sig
    include Domain.HANDLE
    val arg : a
end

type apply = (module APPLY)

type request = [
  | `Domains of Domain.k list
  | `Apply of apply
  | `Ping
  | `Cancel of rqid
  | `Close
]

let request_printer paren out = 
    let open BatIO in
	function
	  | `Domains keys ->
		()
	  | `Close -> nwrite out "`Close"
b	  | `Apply a -> 
	      printf out "`Apply (_, _)"
	  | `Ping -> nwrite out "`Ping"
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

type message = [
    | `Request of rqid * request
    | `Response of rqid * response
]

let message_printer paren out =
    let open BatIO in
	function
	  | `Request (rqid, rq) ->
	      printf out "Request (%d, %a)" 
		  rqid
		  (request_printer false) rq
	  | `Response (rqid, resp) ->
	      printf out "Response (%d, %a)"
		  rqid
		  (response_printer false) resp

type t = {
    in_ch : IO.input_channel;
    out_ch : IO.output_channel;
    mutable next_rqid : rqid;

    remote_domains : Domain.Map.t signal;
    local_domains : Domain.Map.t signal;

    pending_remote : (rqid, response mvar) Hashtbl.t;
    pending_local : (rqid, response lwt) Hashtbl.t;
    local_mgr : Mgr.t;
    remote_mgr : Mgr.t;
}

module type ARGS = sig
    val local_domains : Domain.Map.t signal
    val in_ch : IO.input_channel
    val out_ch : IO.output_channel
    val addr : Unix.sockaddr
end

module Connection (Args : ARGS) = struct
    open Args

    let write_msg (msg : message) =
	dbg "write_msg: %a" (pprint message_printer) msg >>= fun () ->
	IO.write_value out_ch msg

    let read_msg () : message lwt =
	IO.read_value in_ch >>= fun msg ->
	dbg "read_msg: %a" (pprint message_printer) msg;
	return msg

    let remote_domains, set_remote_domains = Signal.create Domain.Map.empty

    module RemoteDomain (K : Domain.KEY) : Domain.DOMAIN = struct
	include K
	let invoke (type a_) (type r_) h a =
	    let module Apply = struct
		include K
		type a = a_
		type r = r_
		let h = h
		let arg = a
	    end in
	    request (`Apply (module Apply : APPLY))
    end

    let make_remote_domain k =
	let module K = (val k : Domain.KEY) in
	let module R = RemoteDomain(K) in
	(module R : Domain.DOMAIN)

	
    let do_request rq = 
	let rqid = !next_rqid in
	incr next_rqid;
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
		  fail (NcExn.immigrate exn))
	    (fun () ->
		return (Hashtbl.remove conn.pending_remote rqid))

    let request rq =
	Mgr.task local_mgr (fun () -> do_request conn rq)

    let close () =
	detach (fun () -> request `Close);
	Mgr.close local_mgr

    let shutdown () =
	IO.close in_ch >>= fun () ->
	IO.close out_ch

    let unit = Obj.repr ()

    let handle_request = function
	| `Close -> 
	    detach (fun () -> 
		(close conn
		 <&> Mgr.close conn.local_mgr) >>= fun () ->
		shutdown conn);
	    return unit
	| `Domains keys ->
	    let ekeys = BatList.enum keys in
	    let edoms = Enum.map make_remote_domain ekeys in
	    let doms = Domain.Map.of_enum edoms in
	    set_remote_domains doms;
	    return unit
	| `Apply apply ->
	    let module Apply = (val apply : APPLY) in
	    let locals = Signal.value local_domains in
	    let dom = Domain.Map.find Apply.key in
	    let module Domain = (val dom : DOMAIN with type d = Apply.d) in
	    Domain.invoke Apply.h Apply.arg >>= fun r ->
	    return (Obj.repr r)
	| `Ping -> 
	    return unit
	| `Cancel rqid -> 
	    let t = Hashtbl.find conn.pending_local rqid in
	    match state t with
	    | Sleep -> begin
		cancel t;
		match state t with
		| Fail Canceled -> return unit
		| _ -> fail (Failure "cancelling failed")
	    end
	    | _ -> fail (Failure "not cancellable")
	    
    let handle_message = function
	| `Response (rqid, resp) ->
	    let p = pending_remotes in
	    if Hashtbl.mem p rqid then 
		let mv = Hashtbl.find p rqid in
		MVar.put mv resp
	    else
		return ()
	| `Request (rqid, rq) ->
	    Mgr.task remote_mgr (fun () ->
		let t = handle_request rq in
		Hashtbl.add pending_local rqid t;
		finalize (const t)
		    (fun () -> 
			return (Hashtbl.remove pending_local rqid))
		>>= fun resp ->
		write_msg (`Response (rqid, resp)))

    let rec read_loop () =
	read_msg () >>= fun msg ->
	detach (fun () -> 
	    dbg "handling: %a" (pprint message_printer) msg >>= fun () ->
	    handle_message msg);
	read_loop ()

    let _ = detach read_loop

    let notify_local_domains m =
	
	    
    let create chans =
	let conn = make chans in
	detach (fun () -> read_loop conn);
	detach (fun () -> request conn (`IAm local_domain));
	conn
	    
    let connect addr = 
	IO.open_connection addr >>= fun chans ->
	return (create chans)


end

module Listener = struct
    type t = IO.server
	    
    let create_listener addr = 
	IO.establish_server addr
	    (fun chans ->
		ignore (create chans))
	    
end

let apply conn h v =
    request conn (`Apply (Obj.magic h, Obj.repr v)) >>= fun o ->
    return (Obj.obj o)
	
