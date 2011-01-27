
open Type

module type KEY = sig
    include TYPE2
    val generate : unit -> ('a, 'b) t2
    val typematch : ('a1, 'b1) t2 -> ('a2, 'b2) t2 -> 
        (('a1, 'a2) teq * ('b1, 'b2) teq) option
    module Cast2(T : TYPE2) : sig
        val typematch : ('a1, 'b1) t2 -> ('a2, 'b2) t2 ->
            (('a1, 'b1) T.t2, ('a2, 'b2) T.t2) teq option
    end
end
module type S = sig
    module K : KEY
    module V : TYPE2

    type t
    val empty : t
    val put : t -> ('a, 'b) K.t2 -> ('a, 'b) V.t2 -> t
    val get : t -> ('a, 'b) K.t2 -> ('a, 'b) V.t2
    val have : t -> ('a, 'b) K.t2 -> bool

    val delete : t -> ('a, 'b) K.t2 -> t

    type iter_fn = {
        visit : 'a 'b . ('a, 'b) K.t2 -> ('a, 'b) V.t2 -> unit
    }
    val iter : t -> iter_fn -> unit

        (*
    type joiner = {
        join : 'a 'b . ('a, 'b) K.t -> ('a, 'b) V.t -> ('a, 'b) V.t -> ('a, 'b) V.t
    }

    val union : joiner -> t -> t -> t
        *)
end
