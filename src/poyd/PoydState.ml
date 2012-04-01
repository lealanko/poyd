open PoydDefs

module Servant = PoydServantStub

type t = {
    run : Phylo.r;
    rng : PoyRandom.t;
}

let create () = {
    run = Phylo.empty ();
    rng = PoyRandom.make [| truncate (Unix.time ()) |];
}

let send s w = 
    Servant.set_run w s.run >>= fun () ->
    Servant.set_rng w s.rng

let receive w =
    Servant.get_run w >>= fun run ->
    Servant.get_rng w >>= fun rng ->
    return { run; rng }
