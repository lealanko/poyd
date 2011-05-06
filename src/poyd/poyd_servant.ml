open Lwt
open FundLwt
open Fund


module Master = PoydMasterStub

let impl = PoydServantImpl.servant

module Maker = PoydServantStub.Make(PoydServantImpl)

let stub = Maker.create impl

let port = 7654

let main () =
    connect ~host:"localhost" ~port () >>= fun conn ->
    get_root "poyd-master" >>= fun master ->
    Master.register_servant master stub >>= fun () ->
    halt ()

let () = Lwt_main.run (main ())
