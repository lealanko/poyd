

open NcPrelude
open NcDefs

let make () = (module struct
    module Seq = Lwt_sequence
    type link = port Seq.node
    let ports = Seq.create ()

    module RouteTable = PolyMap.MakeGlobal(struct
        type ('a, 'b) t2 = port
    end)

    let route_table = ref RouteTable.empty

    let query_ports id qid =
        let ports = Seq.fold_r (fun a l -> a :: l) ports [] in
        let query_port p =
            let module P = (val p : PORT) in
            catch (fun () -> P.query_id id qid)
                (fun _ -> return false) in
        Lwt_list.filter_p query_port ports >>= function 
          | [] -> fail Not_found
          | p :: _ -> return p

    let lookup_id (type a) (id : a id) (qid : qid) : port lwt = 
        try
            return (RouteTable.get !route_table id)
        with
        | Not_found ->
            query_ports id qid >>= fun p ->
            route_table := RouteTable.put !route_table id p;
            return p

    module QIdSet = Set.Make(Uuidm)

    let pending_queries = ref QIdSet.empty

    let query_id (type a) (id : a id) qid = 
        if QIdSet.mem qid !pending_queries
        then return false
        else finalize
            (fun () ->
                pending_queries := QIdSet.add qid !pending_queries;
                try_bind 
                    (fun () -> lookup_id id qid)
                    (fun p -> return true)
                    (fun exn -> return false))
            (fun () ->
                pending_queries := QIdSet.remove qid !pending_queries;
                return ())

    let link (port : port) : link lwt =
        return (Seq.add_r port ports)

    let unlink (l : link) : unit lwt =
        let p = Lwt_sequence.get l in
        let visit k v = if v == p then 
                route_table := RouteTable.delete !route_table k in
        RouteTable.iter !route_table { RouteTable.visit };
        Seq.remove l;
        return ()

            
    let get_root id =
        lookup_id id (Uuidm.create `V4) >>= fun port ->
        let module Port = (val port : PORT) in
        Port.get_root id

    let apply id h a =
        lookup_id id (Uuidm.create `V4) >>= fun port ->
        let module Port = (val port : PORT) in
        Port.apply id h a
        
        

end : ROUTER)
