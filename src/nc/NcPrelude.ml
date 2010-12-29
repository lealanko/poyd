module B = Batteries_uni

include Lwt

include NcLwt

include B.Std

module IO = Lwt_io

module Stream = Lwt_stream
type 'a stream = 'a Stream.t

module MVar = Lwt_mvar
type 'a mvar = 'a MVar.t

module Cond = Lwt_condition
type 'a cond = 'a Cond.t

module Seq = NcSequence
type 'a seq = 'a Seq.t

module List = B.List

module Enum = B.Enum
type 'a enum = 'a Enum.t

module Map = B.Map
type ('k, 'v) map = ('k, 'v) Map.t

module Hashtbl = B.Hashtbl
type ('k, 'v) hashtbl = ('k, 'v) Hashtbl.t

type obj = Obj.t


module Option = BatOption

module Mutex = Lwt_mutex

module Event = React.E
module Signal = React.S

type 'a signal = 'a Signal.t
type 'a event = 'a Event.t

module Set = BatSet
type 'a set = 'a Set.t

type sockaddr = Unix.sockaddr
