open PoydPrelude
open PoydDefs

open FundType

module Client = PoydClientStub
module Servant = PoydServantStub

type 'r msg =
    | RegisterServant of Servant.t * (unit, 'r) teq
    | CreateTask of Client.t * ((script list, unit) handle, 'r) teq

type t = {
    hdl : 'r . ('r msg, 'r) handle
}

let register_servant master servant =
    master.hdl $ RegisterServant(servant, eq_refl)

let create_task master client =
    master.hdl $ CreateTask(client, eq_refl)

module Make(Master : MASTER
            with module Client = Client 
                with module Servant = Servant) = 
struct
    open Master
    let (>:) t teq = 
        Lwt.map (cast teq) t

    let f m = function
        | RegisterServant(servant, teq) ->
            register_servant m servant >: teq
        | CreateTask(client, teq) ->
            create_task m client >: teq

    let make m =
        {
            hdl = fun (type r) -> Fund.publish (f m)
        }
end
