open NcPrelude
open NcDefs
module type S = sig
    include PORT
    val close : unit -> unit lwt
    val abort : unit -> unit lwt
end

type t = (module S)

module type ARGS = sig
    val in_ch : IO.input_channel
    val out_ch : IO.output_channel
    module LocalPort : PORT
end
