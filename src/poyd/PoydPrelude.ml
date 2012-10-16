include FundPrelude
include Fund
include FundType


(* ocaml 3.12: let magic x = x *)
(* magic hack that is required for 4.00 *)
let magic = Obj.magic
