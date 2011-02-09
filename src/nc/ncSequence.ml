open Lwt
open NcLwt

include Lwt_sequence

let push s v = add_r v s
let try_pop = take_opt_l

let push_cont_value s k v = 
    let node = push s v in
    on_cancel_w k (fun () -> remove node)

let push_cont s k = 
    push_cont_value s k k

let take_all seq =
    let l = ref [] in
    iter_node_r (fun node ->
	l := get node :: !l;
	remove node) seq;
    !l

let iter_s f seq =
    fold_r (fun e k () -> f e >>= k) seq return ()
