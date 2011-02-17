
(* A hash table that does not keep the values alive. *)

module type S = sig
    type key
    type 'a t
    val create : int -> 'a t
    val remove : 'a t -> key -> unit
    val get : 'a t -> key -> 'a
    val set : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
end

module Make (H : Hashtbl.HashedType) : S with type key = H.t

module MakeDef(T : sig type t end) : S with type key = T.t
