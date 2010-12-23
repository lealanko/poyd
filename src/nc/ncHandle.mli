open NcPrelude

type ('a,'r) t

val create : ('a -> 'r lwt) -> ('a, 'r) t

val apply : ('a, 'r) t -> 'a -> 'r lwt
