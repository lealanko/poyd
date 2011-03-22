
open NcPrelude

type ('a, 'r) t = {
    domain : NcConnection.domain;
    local_handle : ('a, 'r) NcLocalHandle.t;
}
let apply (h : ('a, 'r) t) (v : 'a) : 'r lwt =
    if h.domain = NcConnection.local_domain 
    then NcLocalHandle.apply h.local_handle v
    else
        let conn = NcConnection.lookup h.domain in
        NcConnection.apply conn h.local_handle v

let local local_handle = {
    domain = NcConnection.local_domain;
    local_handle
}

let create impl = local (NcLocalHandle.create impl)

let wrap f = let h = create f in apply h

let wrap2 f = 
    let h = create (fun (a, b) -> f a b) in
    fun a b -> apply h (a, b)

let wrap3 f
    let h = create (fun (a, b, c) -> f a b c) in
    fun a b c -> apply h (a, b, c)
