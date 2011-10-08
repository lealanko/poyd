open PoydDefs

open Fund
open FundType

type 'r msg =
    | RequestFile of string * (string, 'r) teq
    | OutputStatus of Status.c * string * (unit, 'r) teq
    | SetInformationOutput of string option * (unit, 'r) teq
    | ExplodeFilenames of Parser.filename list * (string list, 'r) teq
    | GetMargin of string option * (int, 'r) teq
    | SetMargin of string option * int * (unit, 'r) teq
    | WaitFinish of (unit, 'r) teq
    | ExecuteOutput of output list * (unit, 'r) teq

type t = { 
    name : string;
    hdl : 'r . ('r msg, 'r) handle 
}

let get_name c =
    return c.name

let request_file c path =
    c.hdl $ RequestFile (path, eq_refl)

let output_status c t msg =
    c.hdl $ OutputStatus (t, msg, eq_refl)

let set_information_output c filename =
    c.hdl $ SetInformationOutput (filename, eq_refl)

let explode_filenames c files =
    c.hdl $ ExplodeFilenames (files, eq_refl)

let get_margin c filename = 
    c.hdl $ GetMargin (filename, eq_refl)

let set_margin c filename margin =
    c.hdl $ SetMargin (filename, margin, eq_refl)

let wait_finish c =
    c.hdl $ WaitFinish (eq_refl)

let execute_output c o =
    c.hdl $ ExecuteOutput (o, eq_refl)

module Make(Client : CLIENT) = struct
    open Client
    let (>:) t teq = 
        Lwt.map (cast teq) t

    let f c = function
        | RequestFile (path, teq) -> 
            request_file c path >: teq
        | OutputStatus (t, msg, teq) -> 
            output_status c t msg >: teq
        | SetInformationOutput (filename, teq) ->
            set_information_output c filename >: teq
        | ExplodeFilenames (files, teq) ->
            explode_filenames c files >: teq
        | GetMargin (filename, teq) ->
            get_margin c filename >: teq
        | SetMargin (filename, margin, teq) ->
            set_margin c filename margin >: teq
        | WaitFinish (teq) ->
            wait_finish c >: teq
        | ExecuteOutput (o, teq) ->
            execute_output c o >: teq

    let create c =
        get_name c >>= fun name ->
        return { 
            name = name;
            hdl = fun (type r) -> Fund.publish (f c);
        }
end
