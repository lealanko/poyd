open FundPrelude
open FundDefs
open FundType

exception ConnectionError of exn

let () = FundExnMapper.register (ConnectionError End_of_file)

module L = (val FundLog.make "FundConnection" : FundLog.S)

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

let pp f fmt =
    Format.fprintf f ("@[" ^^ fmt ^^ "@]@ ")


let pr_any = FundLog.pr_any

let pr_qid f qid =
    pp f "\"%s\"" (Uuidm.to_string qid)
let pr_id f v =
    pr_qid f (FundPolyMap.UuidKey.to_uuid v)

let pr_struct f mems =
    pp f "struct@;<1 1>@[%t@]end" 
        (fun f ->
            List.iter (fun p -> p f) mems)

let memp n pr v f =
    pp f "let %s = %a@ " n pr v

let pr_idq f idq =
    let module IdQuery = (val idq : ID_QUERY) in
    pr_struct f [
        memp "id" pr_id IdQuery.id;
        memp "qid" pr_qid IdQuery.qid]

module type APPLY = sig
    type d
    type a 
    type r
    val id : d id
    val handle : (d, a, r) local_handle
    val arg : a
end

type 'r apply = (module APPLY with type r = 'r)

let pr_apply (type r_) f app =
    let module App = (val app : APPLY with type r = r_) in
    pr_struct f [
        memp "id" pr_id App.id;
        memp "handle" pr_any App.handle;
        memp "arg" pr_any App.arg]

type 'r request =
    | QueryId of id_query * ('r, bool) teq
    | GetRoot of 'r id 
    | Apply of 'r apply
    | Ping of ('r, unit) teq

let pr_request f = function
    | QueryId(idq,_) -> pp f "QueryId(%a,_)" pr_idq idq
    | GetRoot(id) -> pp f "GetRoot(%a)" pr_id id
    | Apply(app) -> pp f "Apply(%a)" pr_apply app
    | Ping(_) -> pp f "Ping(_)"


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

    let pr f = function
        | Request(_) -> pp f "Request(_)"
        | Close -> pp f "Close"
        | Cancel(_) -> pp f "Cancel(_)"
        | Response(_) -> pp f "Response(_)"

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
    val name : string
    val in_ch : IO.input_channel
    val out_ch : IO.output_channel
    module LocalPort : PORT
end

let make_name name in_ch out_ch port = let module M = struct
    module LocalPort = (val port : PORT)
        
    module OutEntry = struct
        type ('a, 'b) t2 = ('a, exn) result mvar
    end

    module OutMap = FundPolyMap.MakeLocal(OutEntry)

    module LocalMsgId = struct
        type 'a t1 = ('a, unit) OutMap.K.t2
    end


    module InEntry = struct
        type ('a, 'b) t2 = 'a lwt
    end

    module InMap = FundPolyMap.MakeLocal(InEntry)

    module RemoteMsgId = struct
        type 'a t1 = ('a, unit) InMap.K.t2
    end

    module OutMsg = Message(LocalMsgId)(RemoteMsgId)

    module InMsg = Message(RemoteMsgId)(LocalMsgId)

    let out_map = ref OutMap.empty
    let in_map = ref InMap.empty

    let with_tmp_file pfx sfx fn =
        let tmp_path = Filename.temp_file pfx sfx
        in
        finalize (fun () -> fn tmp_path)
            (fun () ->
                return (Sys.remove tmp_path))

    let bracket pre thunk post =
        let x = pre ()
        in
        let res = wrap thunk x
        in
        post x;
        ok res

    let copy_channel ic oc len64 =
        let bufsz = Int64.of_int (IO.default_buffer_size ())
        in
        let rec loop len = if len = 0L
            then return ()
            else let sz = min len bufsz in
                 Lwt_io.read ~count:(Int64.to_int sz) ic >>= fun s ->
                 if s = ""
                 then fail End_of_file
                 else begin 
                     IO.write oc s >>= fun () ->
                     loop (Int64.sub len (Int64.of_int (String.length s)))
                 end
        in
        loop len64

            
    let write_big_value x oc =
        match wrap (Marshal.to_string x) [] with
        | Ok s -> begin
            IO.BE.write_int64 oc (Int64.of_int (String.length s)) >>= fun () ->
            IO.write oc s
        end
        | Bad Out_of_memory -> 
            with_tmp_file "fund" ".out" (fun tmp_path ->
                bracket 
                    (fun () -> open_out_bin tmp_path)
                    (fun tmp_oc -> Marshal.to_channel tmp_oc x [])
                    close_out;
                IO.with_file ~mode:IO.input tmp_path (fun tmp_ic -> 
                    IO.length tmp_ic >>= fun len64 ->
                    L.dbg "writing message length %Ld" len64 >>= fun () ->
                    IO.BE.write_int64 oc len64 >>= fun () ->
                    L.dbg "copying %Ld bytes from tmp file %s" len64 tmp_path >>= fun () ->
                    copy_channel tmp_ic oc len64))
        | Bad exn ->
            fail exn

    let read_exactly ic len =
        let s = String.create len
        in
        IO.read_into_exactly ic s 0 len >>= fun () ->
        return s

    let read_big_value ic = 
        L.dbg "reading message length" >>= fun () ->
        IO.BE.read_int64 ic >>= fun len64 ->
        L.dbg "got length: %Ld" len64 >>= fun () ->
        if len64 <= Int64.of_int Sys.max_string_length
        then begin
            read_exactly ic (Int64.to_int len64) >>= fun s ->
            return (Marshal.from_string s 0)
        end
        else 
            with_tmp_file "fund" ".in" (fun tmp_path ->
                L.dbg "copying %Ld bytes to tmp file %s" len64 tmp_path >>= fun () ->
                IO.with_file ~mode:IO.output tmp_path 
                    (fun tmp_oc -> copy_channel ic tmp_oc len64) 
                >>= fun () ->
                L.dbg "Unmarshalling from tmp file %s" tmp_path >>= fun () ->
                let v = bracket
                    (fun () -> open_in_bin tmp_path)
                    Marshal.from_channel
                    close_in
                in return v)
            

    let write_msg (msg : OutMsg.t) =
        catch (fun () ->
            IO.atomic (write_big_value msg) out_ch)
            (fun exn ->
                fail (ConnectionError exn))

    let write_msg msg =
        L.trace (fun () -> write_msg msg) "write_msg %a" OutMsg.pr msg

    let read_msg () : InMsg.t lwt =
        catch (fun () ->
	    IO.atomic read_big_value in_ch)
        (fun exn ->
            L.dbg "Caught exn: %s" (Printexc.to_string exn) >>= fun () ->
            fail (ConnectionError exn))

    let read_msg () =
        L.trace ~pr:InMsg.pr read_msg "read_msg"

    module OutboundMgr = 
        (val FundTaskMgr.make (Printf.sprintf "%s:Outbound" name)
                : FundTaskMgr.S)
    module InboundMgr = 
        (val FundTaskMgr.make (Printf.sprintf "%s:Inbound" name)
                : FundTaskMgr.S)

    let rec do_request_out rq =
        let id = OutMap.K.generate () in
        let mv = MVar.create_empty () in
        out_map := OutMap.put !out_map id mv;
        finalize (fun () ->
           (*  L.dbg "send request: %a" pr_request rq >>= fun () -> *)
            let msg = OutMsg.mk_request rq id in
            block (write_msg msg) >>= fun () ->
            let rec take () = catch
                (fun () -> MVar.take mv)
                (function 
                  | Canceled ->
                      no_cancel (write_msg (OutMsg.mk_cancel id)) >>= fun _ ->
                      take ()
                  | exn -> fail exn) in
            take () >>= run_result)
            (fun () ->
                out_map := OutMap.delete !out_map id;
                return ())

    let do_request_out rq =
        L.trace (fun () -> do_request_out rq) "do_request_out %a" pr_request rq

    let request_out rq =
	OutboundMgr.task (fun () -> do_request_out rq)

    let request_out rq =
        L.trace (fun () -> request_out rq) "request_out %a" pr_request rq

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

    let query_id id qid = 
        L.trace2 "query_id" query_id id ~p1:pr_id qid ~p2:pr_qid

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

    let handle_request rq =
	L.trace (fun () -> handle_request rq) "handle_request %a" pr_request rq

    let handle_message (msg : InMsg.t) : unit lwt = 
        let open InMsg in 
            match msg with
	    | Response respm -> begin
                let module R = (val respm : RESPONSE) in
                try
                    let mv = OutMap.get !out_map R.return_id in
                    out_map := OutMap.delete !out_map R.return_id;
                    let ret = match R.retval with 
                        | Ok v -> Ok v
                        | Bad exn -> Bad (FundExnMapper.immigrate exn) in
                    MVar.put mv ret
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
                OutboundMgr.close ()
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
                 
    let mgr_finish = OutboundMgr.finish <&> InboundMgr.finish   


    (* This contraption makes sure that if finish is cancelled, then
       mgr_finish is cancelled too, but if mgr_finish is cancelled
       independently, then finish returns normally. *)
    let finish = 
        finalize (fun () -> 
            wait_thread mgr_finish)
            (fun () ->
                Lwt_io.close in_ch <&> Lwt_io.close out_ch)

    let close () =
        (* First ensure we won't send any more apply requests. *)
        OutboundMgr.close () >>= fun () ->
        (* Then send the close request. The request is our promise we
           won't call further, the response is the peer's promise they
           won't either. *)
        write_msg OutMsg.Close >>= fun () ->
        (* Now we can stop accepting incoming calls. *)
	InboundMgr.close () >>= fun () -> 
        (* Finally wait until all remaining calls are processed. *)
        finish >>= fun () ->
        (* And close the handles. This triggers on_shutdown. *)
        Lwt_io.close in_ch <&> Lwt_io.close out_ch

    let abort () =
        Lwt_io.abort in_ch <&> Lwt_io.abort out_ch

    let _ =
        let on_read_exn exn =
            L.dbg "read exn" >>= fun () ->
            (* Ensure no new remote requests are made. *)
            OutboundMgr.close () >>= fun () ->
            (* Abort all the current ones. *)
            let visit rqid mv =
                detach (fun () ->
                    L.dbg "cancel request %a" pr_any rqid >>= fun () ->
                    Lwt_mvar.put mv (Bad (ConnectionError exn))) in
            OutMap.(iter !out_map { visit });
            (* Send cancel to all tasks that are still running.
               This should just be inbound. 
            cancel mgr_finish;*)
            cancel InboundMgr.finish;
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
end in return (module M : CONNECTION)

let make ?addr in_ch out_ch port =
    let open Lwt_unix in
        (match addr with 
        | None -> return ""
        | Some (ADDR_UNIX path) -> 
            return path
        | Some (ADDR_INET (a, p)) ->
            try_bind
                (fun () -> gethostbyaddr a)
                (fun host -> return host.h_name)
                (fun _ ->
                    return (Unix.string_of_inet_addr a))
            >>= fun aname ->
            return (Printf.sprintf "%s:%d" aname p))
        >>= fun name ->
        make_name name in_ch out_ch port
        
let make ?addr in_ch out_ch port =
    L.trace (fun () -> make ?addr in_ch out_ch port) "make"
