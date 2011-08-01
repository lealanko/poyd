open PoydDefs

val parallel_pipeline : 
    PoydWorkerPool.t -> 
    PoydClientStub.t ->
    PoydState.t -> 
    int -> 
    script list -> script list -> 
    PoydState.t lwt

