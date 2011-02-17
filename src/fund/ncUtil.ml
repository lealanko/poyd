
type lwt = Lwt.t

module U = Lwt_unix

type fd = U.file_descr

let rec read_bytes_to (fd : fd) (buf : string) (ofs : int) (len : int) =
    U.read fd buf ofs len >>= fun got ->
    if got = len
    then return ()
    else if got = 0
    then raise End_of_file
    else read_bytes_to fd buf (ofs + got) (len - got)

let rec write_bytes_from (fd : fd) (buf : string) (ofs : int) (len : int) =
    U.write fd buf ofs len >>= fun written ->
    if written = len
    then return ()
    else write_bytes_from fd buf (ofs + written) (len - written)



let read_bytes (fd : fd) (n : int) : string lwt =
    let buf = String.create n in
    read_bytes_to fd buf 0 n >>= fun () ->
    return buf

let write_bytes (fd : fd) (buf : string) : unit lwt =
    write_bytes_from fd buf 0 (String.length buf)


module M = Marshal

let input_value (fd : fd) : 'a lwt =
    let hlen = M.header_size in
    read_bytes fd hlen >>= fun hdr ->
    let dlen = M.data_size hdr 0 in
    let buf = String.create (hlen + dlen) in
    let () = String.blit hdr 0 buf 0 hlen in
    read_bytes_to fd buf hlen dlen >>= fun () ->
    return (Marshal.from_string buf 0)

let output_value (fd : fd) v : unit lwt =
    let buf = M.to_string v [] in
    write_bytes fd buf
