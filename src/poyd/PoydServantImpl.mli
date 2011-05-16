open PoydDefs

include SERVANT with module Client = PoydClientStub

val servant : t
