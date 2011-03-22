open FundPrelude
include module type of FundTaskMgrDefs

module Make(Arg : UNIT) : S
