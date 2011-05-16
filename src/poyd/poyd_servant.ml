open Lwt
open FundLwt
open Fund


module Master = PoydMasterStub

let impl = PoydServantImpl.servant

module ServantStub = PoydServantStub.Make(PoydServantImpl)

let port = 7654

let main () =
    ServantStub.create impl >>= fun stub ->
    connect ~host:"localhost" ~port () >>= fun conn ->
    get_root "poyd-master" >>= fun master ->
    Master.register_servant master stub >>= fun () ->
    halt ()

let () = Lwt_main.run (main ())
