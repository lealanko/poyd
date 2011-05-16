open PoydPrelude
open PoydDefs

module Client = PoydClientStub
module Servant = PoydServantStub

type msg =
    | RegisterServant of Servant.t
    | RunTask of Client.t * script list

type t = (msg, unit) handle

let register_servant master servant =
    master $ RegisterServant(servant)

let run_task master client cmds =
    master $ RunTask(client, cmds)

module Make(Master : MASTER
            with module Client = Client 
                with module Servant = Servant) = 
struct
    open Master

    let f m = function
        | RegisterServant(servant) ->
            register_servant m servant
        | RunTask(client, cmds) ->
            run_task m client cmds

    let make m =
        Fund.publish (f m)
end
