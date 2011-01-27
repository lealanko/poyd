
open NcPrelude
include PolyMapDefs
open Type
module type FULL_KEY = sig
    include KEY
    module Ord : Interfaces.OrderedType
    val ord : ('a, 'b) t2 -> Ord.t (* Must be injective! *)
end

module KeyBase (O : Interfaces.OrderedType) = struct
    module Ord = O
    type ('a, 'b) t2 = Ord.t
    let ord k = k
    let typematch (type a1) (type b1) (type a2) (type b2) k1 k2 =
        if Ord.compare (ord k1) (ord k2) = 0 then
            Some (unsafe_eq (), unsafe_eq ())
        else
            None
    module Cast2(T : TYPE2) = struct
        module TO = Type2(T)
        let typematch k1 k2 =
            match typematch k1 k2 with
            | None -> None
            | Some (eq1, eq2) -> Some (TO.cong eq1 eq2)
    end
end

module UuidKey = struct
    include KeyBase(Uuidm)
    let generate () = 
        Uuidm.create `V4
    let unsafe_of_string s =
        Uuidm.create (`V5 (Uuidm.nil, s))
end


module IntKey (Unit : UNIT) = struct
    include KeyBase(Int)
    let next = ref 0
    let generate () = 
        incr next; 
        !next
end


module Make(Key : FULL_KEY) (Value : TYPE2) = struct
    module K = Key
    module V = Value

    module M = Legacy.Map.Make(K.Ord)

    module type ENTRY = sig
        type a
        type b
        val k : (a, b) K.t2
        val v : (a, b) V.t2
    end
    type entry = (module ENTRY)

    let mk_entry (type a_) (type b_) (k_ : (a_, b_) K.t2) (v_ : (a_, b_) V.t2) =
        let module Entry = struct
            type a = a_
            type b = b_
            let k = k_
            let v = v_
        end in 
        (module Entry : ENTRY)

    type t = entry M.t

    module TV = Type2(V)

    let empty = 
        M.empty

    let put m k v = 
        M.add (K.ord k) (mk_entry k v) m

    let get m k = 
        let entry = M.find (K.ord k) m in
        let module Entry = (val entry : ENTRY) in
        match K.typematch Entry.k k with
        | None -> raise Not_found (* Impossible, if K.ord is injective *)
        | Some (eqa, eqb) ->
            cast (TV.cong eqa eqb) Entry.v

    let have m k = 
        M.mem k m

    let delete m k =
        M.remove (K.ord k) m

    type iter_fn = {
        visit : 'a 'b . ('a, 'b) K.t2 -> ('a, 'b) V.t2 -> unit
    }
    let iter m f = M.iter (fun k v ->
        let module Entry = (val v : ENTRY) in
        f.visit Entry.k Entry.v) m

end

module MakeGlobal(Value : TYPE2) =
    Make(UuidKey)(Value)

module MakeLocal(Value : TYPE2) =
    Make(IntKey(Unit))(Value)
