open NcPrelude
open NcDefs
open Type

include ConnectionDefs

let section = Lwt_log.Section.make "NcConnection"

let dbg fmt = 
    Lwt_log.debug_f ~section fmt

let pprint printer () v =
    let out = BatIO.output_string () in
    let outc = BatIO.cast_output out in
    printer false outc v;
    BatIO.close_out out


module type ID_QUERY = sig
    type d
    val id : d id
    val qid : qid
end

type id_query = (module ID_QUERY)

module type APPLY = sig
    type d
    type a 
    type r
    val id : d id
    val handle : (d, a, r) local_handle
    val arg : a
end

type 'r apply = (module APPLY with type r = 'r)

type 'r request =
    | QueryId of id_query * ('r, bool) teq
    | GetRoot of 'r id 
    | Apply of 'r apply
    | Ping of ('r, unit) teq

module Message(LocalId : TYPE1)(RemoteId : TYPE1) = struct
    module type CANCEL = sig
        type a
        val cancel_id : a LocalId.t1
    end

    module type RESPONSE = sig
        type r
        val return_id : r RemoteId.t1
        val retval : (r, exn) result
    end

    module type REQUEST_MSG = sig
        type r
        val rq : r request
        val return_id : r LocalId.t1
    end

    type t = 
        | Request of (module REQUEST_MSG)
        | Close
        | Cancel of (module CANCEL)
        | Response of (module RESPONSE)

    let mk_cancel (type a_) id =
        Cancel (module struct
            type a = a_
            let cancel_id = id
        end : CANCEL)

    let mk_response (type r_) rid ret =
        Response (module struct
            type r = r_
            let return_id = rid
            let retval = ret
        end : RESPONSE)

    let mk_request (type r_) rq_ rid =
        Request (module struct
            type r = r_
            let rq = rq_
            let return_id = rid
        end : REQUEST_MSG)
                
end


module type ARGS = sig
    val in_ch : IO.input_channel
    val out_ch : IO.output_channel
    module LocalPort : PORT
end

module Make (Args : ARGS) = struct
    open Args

    module OutEntry = struct
        type ('a, 'b) t2 = ('a, exn) result mvar
    end

    module OutMap = PolyMap.MakeLocal(OutEntry)

    module LocalMsgId = struct
        type 'a t1 = ('a, unit) OutMap.K.t2
    end


    module InEntry = struct
        type ('a, 'b) t2 = 'a lwt
    end

    module InMap = PolyMap.MakeLocal(InEntry)

    module RemoteMsgId = struct
        type 'a t1 = ('a, unit) InMap.K.t2
    end

    module OutMsg = Message(LocalMsgId)(RemoteMsgId)

    module InMsg = Message(RemoteMsgId)(LocalMsgId)

    let out_map = ref OutMap.empty
    let in_map = ref InMap.empty

    let write_msg (msg : OutMsg.t) =
	IO.write_value out_ch msg

    let read_msg () : InMsg.t lwt =
	IO.read_value in_ch >>= fun msg ->
	return msg

    module OutboundMgr = TaskMgr.Make(Unit)
    module InboundMgr = TaskMgr.Make(Unit)

    let rec do_request_out rq =
        let id = OutMap.K.generate () in
        let mv = MVar.create_empty () in
        out_map := OutMap.put !out_map id mv;
        finalize (fun () ->
            let msg = OutMsg.mk_request rq id in
            block (write_msg msg) >>= fun () ->
            let rec take () = catch
                (fun () -> MVar.take mv)
                (function 
                  | Canceled ->
                      no_cancel (write_msg (OutMsg.mk_cancel id)) >>= fun _ ->
                      take ()
                  | exn -> fail exn) in
            take () >>= function
              | Ok v ->
                  return v
              | Bad exn ->
                  fail exn)
            (fun () ->
                out_map := OutMap.delete !out_map id;
                return ())

    let request_out rq =
	OutboundMgr.task (fun () -> do_request_out rq)

    let get_root key =
        request_out (GetRoot key)

    let apply (type d_) (type a_) (type r_) id h a =
        let module Apply = struct
            type d = d_
            type a = a_
            type r = r_
            let id = id
            let handle = h
            let arg = a
        end in
        request_out (Apply (module Apply : APPLY with type r = r_))

    module UnitT = struct
        type ('a, 'b) t2 = unit
    end

    let pending_queries = Hashtbl.create 17

    (* An optimization for root/domain discovery: keep track of the
       inbound queries already running, and don't send them outbound:
       since the remote peer asked for it already, bouncing the query
       back can't do any good. *)
    let query_id (type d_) id qid =
        if Hashtbl.mem pending_queries qid
        then 
            return false
        else
            let module IdQ = struct 
                type d = d_ let id = id let qid = qid 
            end in
            request_out (QueryId ((module IdQ : ID_QUERY), eq_refl))

    let shutdown () : unit lwt =
	IO.close in_ch <&> IO.close out_ch


    let handle_request (type r_) : r_ request -> r_ lwt = function
        | GetRoot rkey ->
            LocalPort.get_root rkey
	| Apply apply ->
            let module Apply = (val apply : APPLY with type r = r_) in
            LocalPort.apply Apply.id Apply.handle Apply.arg
	| QueryId(idq, teq) -> 
            let module IdQ = (val idq : ID_QUERY) in
            if Hashtbl.mem pending_queries IdQ.qid
            then 
                return (cast_back teq false)
            else begin
                Hashtbl.add pending_queries IdQ.qid ();
                finalize 
                    (fun () ->
                        LocalPort.query_id IdQ.id IdQ.qid)
                    (fun () ->
                        Hashtbl.remove pending_queries IdQ.qid;
                        return ())
                >>= fun b ->
                return (cast_back teq b)
            end
	| Ping teq -> 
            return (cast_back teq ())
	              
    let handle_message (msg : InMsg.t) : unit lwt = 
        let open InMsg in 
            match msg with
	    | Response respm -> begin
                let module R = (val respm : RESPONSE) in
                try
                    let mv = OutMap.get !out_map R.return_id in
                    out_map := OutMap.delete !out_map R.return_id;
                    MVar.put mv R.retval
                with 
                | Not_found ->
                    (* Response to unknown request *)
		    return ()
            end
	    | Request rqmsg ->
                let module R = (val rqmsg : REQUEST_MSG) in
	        InboundMgr.task (fun () ->
                    try_bind (fun () ->
                        fix (fun t ->
                            in_map := InMap.put !in_map R.return_id t;
                            finalize (fun () ->
                                handle_request R.rq)
                                (fun () ->
                                    in_map := InMap.delete !in_map R.return_id;
                                    return ())))
                        (fun ret -> return (Ok ret))
                        (fun exn -> return (Bad exn))
                    >>= fun ret ->
		    write_msg (OutMsg.mk_response R.return_id ret))
	    | Close -> 
                OutboundMgr.close ();
                return ()
	    | Cancel cm ->
                let module Cancel = (val cm : CANCEL) in
	        let t = InMap.get !in_map Cancel.cancel_id in
	        match state t with
	        | Sleep -> begin
		    cancel t;
		    match state t with
		    | Fail Canceled -> return ()
		    | _ -> fail (Failure "cancelling failed")
	        end
	        | _ -> fail (Failure "not cancellable")
                    
    let finish = 
        finalize (fun () ->
            OutboundMgr.finish <&> InboundMgr.finish)
            (fun () ->
                Lwt_io.close in_ch <&> Lwt_io.close out_ch)

    let close () =
        (* First ensure we won't send any more apply requests. *)
        OutboundMgr.close ();
        (* Then send the close request. The request is our promise we
           won't call further, the response is the peer's promise they
           won't either. *)
        write_msg OutMsg.Close >>= fun () ->
        (* Now we can stop accepting incoming calls. *)
	InboundMgr.close ();
        (* Finally wait until all remaining calls are processed. *)
        finish >>= fun () ->
        (* And close the handles. This triggers on_shutdown. *)
        Lwt_io.close in_ch <&> Lwt_io.close out_ch

    let abort () =
        Lwt_io.abort in_ch <&> Lwt_io.abort out_ch

    let _ =
        let on_read_exn exn =
            (* Ensure no new remote requests are made. *)
            OutboundMgr.close ();
            (* Abort all the current ones. *)
            let visit rqid mv =
                detach (fun () ->
                    Lwt_mvar.put mv (Bad exn)) in
            OutMap.iter !out_map { OutMap.visit };
            (* Send cancel to all tasks that are still running.
               This should just be inbound. *)
            cancel finish;
            return () in
        let rec handle_msg msg =
	    detach (fun () -> 
	        handle_message msg);
	    loop ()
        and loop () = 
            try_bind 
                read_msg
                handle_msg
                on_read_exn in
        detach loop
end
        
let make in_ch out_ch port = 
    let module Args = struct
        let in_ch = in_ch
        let out_ch = out_ch
        module LocalPort = (val port : PORT)
    end in
    let module C = Make(Args) in
    (module C : CONNECTION)
