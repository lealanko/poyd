open FundPrelude
open FundDefs

val make : IO.input_channel -> IO.output_channel -> port -> connection lwt
