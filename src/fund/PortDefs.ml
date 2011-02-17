module type S = sig
    val query_domain : Domain.id -> bool lwt
    val request : 'a Domain.request -> 'a lwt
    val get_root : 'a Domain.root_id -> 'a lwt
end

type t = (module S)
