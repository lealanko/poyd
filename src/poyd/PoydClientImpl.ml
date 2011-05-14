open PoydPrelude
open PoydDefs


type t = unit

let client = ()

let thr = PoydPoy.thread

let output_status _ c msg =
    PoydThread.run thr (fun () ->
        Status.user_message c msg)

let read_channel ch =
    let buf = Buffer.create 16 in
    let s = String.create 4096 in
    let rec read () =
        let len = Pervasives.input ch s 0 4096 in
        if (len > 0) then begin
            Buffer.add_substring buf s 0 len;
            read ()
        end in
    read ();
    Buffer.contents buf

let request_file _ filename =
    PoydThread.run thr (fun () ->
        let ch = open_in_bin filename in
        let c = read_channel ch in
        close_in ch;
        c)

let explode_filenames _ files =
    PoydThread.run thr (fun () ->
        PoyParser.explode_filenames files)

let get_name _ =
    PoydUtil.get_procid ()
