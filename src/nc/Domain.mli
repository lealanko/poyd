open NcPrelude

include module type of DomainDefs

module Map : sig
    type t
    val empty : t
    val add : t -> 'd domain -> t
    val find : t -> 'd key -> 'd domain
    val remove : t -> 'd key -> t
    val of_enum : d enum -> t
    val enum : t -> d enum
end

val handle : 'd key -> ('d, 'a, 'r) handle -> ('a, 'r) h

val local : unit -> local

val root : ('d, string, obj) handle
