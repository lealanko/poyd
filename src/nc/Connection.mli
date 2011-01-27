open NcPrelude

include module type of ConnectionDefs

module Make(Args : ARGS) : S
