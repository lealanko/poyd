open NcPrelude
open NcDefs
open Type

module LocalDomain(Unit : UNIT) : LOCAL_DOMAIN = struct
            
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
    end 

module DomainPort(D : DOMAIN) : PORT = struct
    module RequestValue = struct
        type ('a, 'b) t2 = (module APPLICATION with type d = 'a)
    end
    module Cast = PolyMap.UuidKey.Cast2(RequestValue)

    let query_id idq = 
        let module IdQ = (val idq : ID_QUERY) in
        match Cast.typematch IdQ.id D.id with
        | Some _ -> return true
        | None -> try_bind
            (fun () -> D.get_root IdQ.id)
              (fun _ -> return true)
              (function 
                | Not_found -> return false
                | exn -> fail exn)

    let get_root = D.get_root

    let apply (type r_) app =
        let module R = (val app : APPLICATION with type r = r_) in
        let rq_ = (module R : APPLICATION with type d = R.d) in
        match Cast.typematch R.id D.id with
        | None -> fail Not_found
        | Some eq -> 
            let rq2 = cast eq rq_ in
            let module R2 = (val rq2 : APPLICATION with type d = D.d) in
            D.invoke R2.handle R2.arg
end
