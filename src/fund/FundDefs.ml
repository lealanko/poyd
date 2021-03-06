
open FundPrelude
open FundType

type 'd id = ('d, unit) FundPolyMap.UuidKey.t2

type ('d, 'a, 'b) local_handle = ('d, 'a, 'b) type2

type qid = Uuidm.t

module type PORT = sig
    val query_id : 'a id -> qid -> bool lwt
    val apply : 'd id -> ('d, 'a, 'r) local_handle -> 'a -> 'r lwt
    val get_root : 'a id -> 'a lwt
end

type port = (module PORT)


module type DOMAIN = sig
    type d
    val id : d id
    val invoke : (d, 'a, 'r) local_handle -> 'a -> 'r lwt
    val get_root : 'a id -> 'a lwt
end

type 'd domain = (module DOMAIN with type d = 'd)


module type LOCAL_DOMAIN = sig
    include DOMAIN
    val set_root : 'a id -> 'a -> unit
    val register_handle : ('a -> 'r lwt) -> (d, 'a, 'r) local_handle
    val unregister_handle : (d, 'a, 'r) local_handle -> unit
end

module type LISTENER = sig
    val close : unit -> unit lwt
end

type listener = (module LISTENER)

module type CONNECTION = sig
    include PORT
    val close : unit -> unit lwt
    val abort : unit -> unit lwt
    val finish : unit lwt
end

module type ROUTER = sig
    include PORT
    type link
    val link : port -> link lwt
    val unlink : link -> unit lwt
end

type router = (module ROUTER)

type connection = (module CONNECTION)

