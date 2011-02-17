
module B = Batteries_uni

include Lwt

include FundLwt

include B.Std

module IO = Lwt_io

module Stream = Lwt_stream
type 'a stream = 'a Stream.t

module MVar = Lwt_mvar
type 'a mvar = 'a MVar.t

module Cond = Lwt_condition
type 'a cond = 'a Cond.t

module Seq = FundSequence
type 'a seq = 'a Seq.t

module List = B.List

module Enum = B.Enum
type 'a enum = 'a Enum.t

module Map = B.Map
type ('k, 'v) map = ('k, 'v) Map.t

module Interfaces = B.Interfaces

module Hashtbl = B.Hashtbl
type ('k, 'v) hashtbl = ('k, 'v) Hashtbl.t

type obj = Obj.t

module String = B.String

module Option = B.Option

module Mutex = Lwt_mutex

module Event = React.E
module Signal = React.S

type 'a signal = 'a Signal.t
type 'a event = 'a Event.t

module Set = B.Set
type 'a set = 'a Set.t

type sockaddr = Unix.sockaddr

module type UNIT = sig end
module Unit = struct end

module Legacy = B.Legacy

module Int = B.Int
