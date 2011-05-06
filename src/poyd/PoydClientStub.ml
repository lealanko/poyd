open PoydDefs

open Fund
open FundType

type 'r msg =
    | RequestFile of string * (string, 'r) teq
    | OutputStatus of Status.c * string * (unit, 'r) teq
    | ExplodeFilenames of Parser.filename list * (string list, 'r) teq

type t = { hdl : 'r . ('r msg, 'r) handle }

let request_file c path =
    c.hdl $ RequestFile (path, eq_refl)

let output_status c t msg =
    c.hdl $ OutputStatus (t, msg, eq_refl)

let explode_filenames c files =
    c.hdl $ ExplodeFilenames (files, eq_refl)


module Make(Client : CLIENT) = struct
    open Client
    let (>:) t teq = 
        Lwt.map (cast teq) t

    let f c = function
        | RequestFile (path, teq) -> 
            request_file c path >: teq
        | OutputStatus (t, msg, teq) -> 
            output_status c t msg >: teq
        | ExplodeFilenames (files, teq) ->
            explode_filenames c files >: teq

    let make c =
        { hdl = fun (type r) -> Fund.publish (f c) }
end
