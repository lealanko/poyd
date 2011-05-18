
(* Use a custom PRNG state in order to keep results reproducible even
   when using libraries that might use the standard Random module *)

module Random = PoyRandom
