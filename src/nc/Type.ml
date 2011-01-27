open NcPrelude

type ('a, 'b) teq = Teq

module type TYPE0 = sig
    type t0
end

module type TYPE1 = sig
    type 'a t1
end

module type TYPE2 = sig
    type ('a, 'b) t2
end

module type TYPE2_EQ = sig
    type ('a, 'b) t2a
    type ('a, 'b) t2b
    val eq : (('a, 'b) t2a, ('a, 'b) t2b) teq
end

module CastF(T : TYPE1) = struct
    let cast teq v = Obj.magic v
end

let eq_refl = Teq
let eq_sym _ = Teq
let eq_trans _ _ = Teq
let cast teq v = Obj.magic v
let cast_back teq v = Obj.magic v

type ('d, 'a, 'b) type2

module Type2 (T : TYPE2) = struct
    type d
    let eqd = Teq
    let cong aeq beq = Teq
end

let unsafe_eq () = Teq
