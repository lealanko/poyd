open NcPrelude
open NcDefs

include module type of ConnectionDefs

val make : IO.input_channel -> IO.output_channel -> port -> connection
