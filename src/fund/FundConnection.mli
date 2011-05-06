open FundPrelude
open FundDefs

exception ConnectionError of exn

val make : IO.input_channel -> IO.output_channel -> port -> connection lwt
