
open NcPrelude

let section = Lwt_log.Section.make "NcLocalHandle"

type ('a, 'r) t = int

type ('a, 'r) stub = {
    key : ('a, 'r) t;
    impl : 'a -> 'r lwt
}

module M : sig
    val mk : int -> ('a, 'r) t
    val un : ('a, 'r) t -> int
end = struct
    let mk i = i
    let un i = i
end

module W = Weaktbl.MakeDef(struct type t = int end)

let table : Obj.t W.t = W.create 19

let next_key : int ref = ref 0

let apply (h : ('a, 'r) t) (v : 'a) : 'r lwt =
    let stub = Obj.obj (W.get table h) in
    Lwt_log.debug_f ~section "apply" >>= fun () ->
    stub.impl v

let create impl =
    let key (type a) = !next_key in
    next_key := key + 1;
    let stub = { key; impl } in
    W.set table key (Obj.repr stub);
    key
