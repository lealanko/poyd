type ('a, 'b) teq
type (-'a, +'b) tsub

module type TYPE0 = sig
    type t0
end

module type TYPE1 = sig
    type 'a t1
end

module type TYPE2 = sig
    type ('a, 'b) t2
end

module CastF(T : TYPE1) : sig
    val cast : ('a, 'b) teq -> 'a T.t1 -> 'b T.t1
end

val eq_refl : ('a, 'a) teq
val eq_sym : ('a, 'b) teq -> ('b, 'a) teq
val eq_trans : ('a, 'b) teq -> ('b, 'c) teq -> ('a, 'c) teq
val eq_sub : ('a, 'b) teq -> ('a, 'b) tsub

val sub_refl : ('a, 'a) tsub
val cast : ('a, 'b) teq -> 'a -> 'b
val cast_back : ('a, 'b) teq -> 'b -> 'a

val tcast : ('a, 'b) tsub -> 'a -> 'b

type ('d, 'a, 'b) type2

module Type2 (T : TYPE2) : sig
    type d
    val eqd : ((d, 'a, 'b) type2, ('a, 'b) T.t2) teq
    val cong : ('a1,'a2) teq -> ('b1,'b2) teq -> 
        (('a1,'b1) T.t2, ('a2,'b2) T.t2) teq
end

val unsafe_eq : unit -> ('a, 'b) teq




