open PoydDefs

val parallel_pipeline : 
    PoydWorkerPool.t -> 
    PoydClientStub.t ->
    PoydState.t -> 
    int -> 
    script list -> script list -> 
    PoydState.t lwt

val on_each_tree : 
    PoydWorkerPool.t -> 
    PoydClientStub.t ->
    PoydState.t -> 
    script list -> script list -> 
    PoydState.t lwt

val support : 
    PoydWorkerPool.t -> 
    PoydClientStub.t ->
    PoydState.t -> 
    Methods.support_method ->
    PoydState.t lwt

