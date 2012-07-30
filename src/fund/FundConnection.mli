open FundPrelude
open FundDefs

exception ConnectionError of exn

val make : 
    ?addr:Lwt_unix.sockaddr -> 
    IO.input_channel -> IO.output_channel -> port -> 
    connection lwt
