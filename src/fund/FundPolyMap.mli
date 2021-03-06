
open FundType
include module type of FundPolyMapDefs

module UuidKey : sig
    include KEY
    val unsafe_of_string : string -> ('a, 'b) t2
    val to_uuid : ('a, 'b) t2 -> Uuidm.t
end

module MakeGlobal(V : TYPE2) : 
    S with module V = V and type ('a, 'b) K.t2 = ('a, 'b) UuidKey.t2

module MakeLocal(V : TYPE2) : S with module V = V
