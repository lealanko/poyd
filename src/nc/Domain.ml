open NcPrelude
open NcDefs
open Type

module L = (val Log.make "Domain" : Log.S)

let local () = (module struct
    module HandleValue = struct
        type ('a, 'b) t2 = 'a -> 'b lwt
    end
    module HandleMap = PolyMap.MakeLocal(HandleValue)
        
    module T2 = Type2(HandleMap.K)
    type d = T2.d
    let id = PolyMap.UuidKey.generate ()
        
    let handles = ref HandleMap.empty
        
    let register_handle f =
        let k = HandleMap.K.generate () in
        handles := HandleMap.put !handles k f;
        cast_back T2.eqd k
            
    let invoke k arg =
        let f = HandleMap.get !handles (cast T2.eqd k) in
        f arg
            
            
    module RootValue = struct
        type ('a, 'b) t2 = 'a
    end
    module RootMap = PolyMap.MakeGlobal(RootValue)
        
    let roots = ref RootMap.empty
    let set_root k v = roots := RootMap.put !roots k v
    let get_root k = 
        return (RootMap.get !roots k)
end : LOCAL_DOMAIN)

module DomainPort(D : DOMAIN) : PORT = struct

    let query_id id qid =
        match PolyMap.UuidKey.typematch id D.id with
        | Some _ -> return true
        | None -> try_bind
            (fun () -> D.get_root id)
              (fun _ -> return true)
              (function 
                | Not_found -> return false
                | exn -> fail exn)

    let query_id id qid = 
        L.trace (fun () -> query_id id qid) "query_id"


    let get_root = D.get_root

    let apply (type d) (type a) (type r) 
            (id : d id) (handle : (d, a, r) local_handle) (arg : a) : r lwt =
        let module RequestValue = struct
            type ('d, 'b_) t2 = ('d, a, r) local_handle
        end in
        let module Cast = PolyMap.UuidKey.Cast2(RequestValue) in
        match Cast.typematch id D.id with
        | None -> fail Not_found
        | Some eq -> 
            let handle_ = cast eq handle in
            D.invoke handle_ arg
end

let port d = 
    let module D = (val d : DOMAIN) in
    let module P = DomainPort(D) in
    (module P : PORT)
