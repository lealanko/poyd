open PoydDefs

val run_parallel : 
    PoydWorkerPool.t -> 
    PoydClientStub.t ->
    PoydState.t -> 
    par_method ->
    (PoydState.t * script list) lwt
