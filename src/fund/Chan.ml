open NcPrelude

module type INPUT = sig
    type 'a t
    val read : 'a t -> 'a lwt
end

module type OUTPUT = sig
    type 'a t
    val write : 'a t -> 'a -> unit lwt
end

module type INOUT = sig
    include INPUT
    include OUTPUT with type 'a t := 'a t
end

