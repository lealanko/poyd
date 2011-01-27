
open NcPrelude
open Type

type 'd id = ('d, unit) PolyMap.UuidKey.t2

type ('d, 'a, 'b) local_handle = ('d, 'a, 'b) type2

module type ANY_ID = sig
    type d
    val id : d id
end

type any_id = (module ANY_ID)

module type HANDLE = sig
    include ANY_ID
    type a
    type r
    val handle : ('d, 'a, 'r) local_handle
end

type ('a, 'r) handle = (module HANDLE with type a = 'a and type r = 'r)

module type APPLICATION = sig
    include HANDLE
    val arg : a
end

type 'r application = (module APPLICATION with type r = 'r)

type qid = (unit, unit) PolyMap.UuidKey.t2

module type ID_QUERY = sig
    include ANY_ID
    val qid : qid
end

type id_query = (module ID_QUERY)

module type PORT = sig
    val query_id : id_query -> bool lwt
    val apply : 'a application -> 'a lwt
    val get_root : 'a id -> 'a lwt
end

type port = (module PORT)


module type DOMAIN = sig
    include ANY_ID
    val invoke : (d, 'a, 'r) local_handle -> 'a -> 'r lwt
    val get_root : 'a id -> 'a lwt
end

type 'd domain = (module DOMAIN with type d = 'd)


module type LOCAL_DOMAIN = sig
    include DOMAIN
    val set_root : 'a id -> 'a -> unit
    val register_handle : ('a -> 'r lwt) -> (d, 'a, 'r) local_handle
end

module type LISTENER = sig
    val close : unit -> unit lwt
    val abort : unit -> unit lwt
end

type listener = (module LISTENER)

