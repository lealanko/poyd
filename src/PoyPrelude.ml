
(* Use a custom PRNG state in order to keep results reproducible even
   when using libraries that might use the standard Random module *)

module Random = PoyRandom

let max_int = 0x3fffffff
let min_int = -0x40000000
