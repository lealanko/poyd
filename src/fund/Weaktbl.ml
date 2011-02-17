
module type S = sig
    type key
    type 'a t
    val create : int -> 'a t
    val remove : 'a t -> key -> unit
    val get : 'a t -> key -> 'a
    val set : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
end

module Make (H : Hashtbl.HashedType) = struct
    type key = H.t
    module HT = Hashtbl.Make (H)
    type 'a t = {
	mutable weaks : 'a Weak.t;
	mutable next : int;
	ht : int HT.t
    }

    let create n = {
	weaks = Weak.create n;
	next = 1;
	ht = HT.create n
    }


    let resize t =
	let count = ref 0 in
	let f k n = if Weak.check t.weaks n then count := !count + 1 in
	let _ = HT.iter f t.ht in
	let new_weaks = Weak.create (!count * 2) in
	let g k n i = match Weak.get t.weaks n with
	    | Some v as e -> 
		Weak.set new_weaks i e; 
		HT.replace t.ht k i;
		i + 1
	    | None -> HT.remove t.ht k; i in
	t.next <- HT.fold g t.ht 0;
	t.weaks <- new_weaks

    let check_resize t =
	if t.next = Weak.length t.weaks
	then resize t

    let set t k a = 
	begin
	    HT.remove t.ht k;
	    check_resize t;
	    Weak.set t.weaks t.next (Some a);
	    HT.add t.ht k t.next;
	    t.next <- t.next + 1;
	end

    let remove t k =
	try
	    let n = HT.find t.ht k in
	    Weak.set t.weaks n None;
	    HT.remove t.ht k
	with Not_found -> ()

    let get t k =
	(* Let Not_found propagate *)
	let n = HT.find t.ht k in
	match Weak.get t.weaks n with
	| Some v -> v
	| None -> 
	    HT.remove t.ht k; 
	    raise Not_found

    let mem t k =
	HT.mem t.ht k && Weak.check t.weaks (HT.find t.ht k)
	    
end

module MakeDef(T : sig type t end) =
    Make(struct type t = T.t 
		let equal = (=)
		let hash = Hashtbl.hash
    end)

