open PoydDefs

include MASTER 
    with module Client = PoydClientStub 
    with module Servant = PoydServantStub

val create : unit -> t
