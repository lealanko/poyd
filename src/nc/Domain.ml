open NcPrelude

type key_i = Uuidm.t
type handle_i = int

module rec Defs : module type of DomainDefs 
    with type 'd key = key_i
    and type ('d, 'a, 'r) handle = handle_i
= Defs
include Defs


let handle (type d_) (type a_) (type r_) key h =
    let module H = struct
	type d = d_
	type a = a_
	type r = r_
	let key = key
	let h = h
    end in
      (module H : HANDLE with type a = a_ and type r = r_)

module Map = struct
    type t = (key_i, obj) B.Map.t
    let empty = B.Map.empty
    let add (type dd) t dom =
	let module D = (val dom : DOMAIN with type d = dd) in
	  B.Map.add D.key (Obj.repr dom) t
    let find t key =
	let o = B.Map.find key t in
	  Obj.obj o
    let remove t key = 
	B.Map.remove key t
end

let local () =
    let module L = struct
	type d
	let key = Uuidm.create `V4
	let handles = Hashtbl.create 7
	let roots = Hashtbl.create 7
	let next_h = ref 1
	let register f =
	    let h = !next_h in
	    let _ = incr next_h in
	      Hashtbl.add handles h (Obj.magic f);
	      h
	let unregister h =
	    Hashtbl.remove handles h
	let invoke h a =
	    let f = Hashtbl.find handles h in
	    Obj.obj (f (Obj.repr a))
	let set_root rid v =
	    Hashtbl.add roots rid (Obj.repr v)
	let root rid =
	    Obj.obj (Hashtbl.find roots rid)
	let _ = Hashtbl.add handles 0 (Obj.magic root)
    end in
      (module L : LOCAL)

let root = 0
