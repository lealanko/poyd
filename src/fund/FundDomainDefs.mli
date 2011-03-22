
open NcPrelude

type 'd key
type ('d, 'a, 'r) handle

module type KEY = sig
    type d
    val key : d key
end

type k = (module KEY)

module type HANDLE = sig
    include KEY
    type a
    type r
    val h : (d, a, r) handle
end

type ('a, 'r) h = (module HANDLE with type a = 'a and type r = 'r)

module type DOMAIN = sig
    include KEY
    val invoke : (d, 'a, 'r) handle -> 'a -> 'r lwt
end

type 'd domain = (module DOMAIN with type d = 'd)
type d = (module DOMAIN)

module type LOCAL = sig
    include DOMAIN
    val register : ('a -> 'r lwt) -> (d, 'a, 'r) handle
    val unregister : (d, 'a, 'r) handle -> unit
end

module type REQUEST = sig
    include HANDLE
    val v : a
end

type 'r request = (module REQUEST with type r = 'r)

type local = (module LOCAL)
