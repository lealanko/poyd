
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
