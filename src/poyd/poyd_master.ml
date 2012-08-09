open Lwt
open FundLwt
open Fund

module L = (val FundLog.make "PoydMaster" : FundLog.S)

let checkpoint = ref 60.0

let master_specs = [
    ("-c", Arg.Set_float checkpoint,
     Printf.sprintf
         "Seconds between checkpoints (default %f)" !checkpoint)
]

let _ = PoydArgs.(Arg.parse (common_specs @ master_specs) anon_arg usage)

module MasterStub = PoydMasterStub.Make(PoydMasterImpl)

let impl = PoydMasterImpl.create ~checkpoint_secs:!checkpoint

let stub = MasterStub.make impl

let port = !PoydArgs.port

let main () =
    listen ~port () >>= fun () ->
    L.info "Listening on port %d" port >>= fun () ->
    set_root "poyd-master" stub >>= fun () ->
    halt ()

let () = Lwt_main.run (main ())
