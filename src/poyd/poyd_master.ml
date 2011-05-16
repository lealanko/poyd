open Lwt
open FundLwt
open Fund


module MasterStub = PoydMasterStub.Make(PoydMasterImpl)

let impl = PoydMasterImpl.create ()

let stub = MasterStub.make impl


let port = 7654

let main () =
    listen ~port () >>= fun () ->
    set_root "poyd-master" stub >>= fun () ->
    halt ()

let () = Lwt_main.run (main ())
