
include Lwt

include FundLwt

include BatPervasives

module IO = Lwt_io

module Stream = Lwt_stream
type 'a stream = 'a Stream.t

module MVar = Lwt_mvar
type 'a mvar = 'a MVar.t

module Cond = Lwt_condition
type 'a cond = 'a Cond.t

module Seq = FundSequence
type 'a seq = 'a Seq.t

module List = Batteries.List

module Enum = Batteries.Enum
type 'a enum = 'a Enum.t

module Map = Batteries.Map
type ('k, 'v) map = ('k, 'v) Map.t

module Interfaces = Batteries.Interfaces

module Hashtbl = Batteries.Hashtbl
type ('k, 'v) hashtbl = ('k, 'v) Hashtbl.t

type obj = Obj.t

module String = Batteries.String

module Option = Batteries.Option

module Mutex = Lwt_mutex

module Set = Batteries.Set
type 'a set = 'a Set.t

type sockaddr = Unix.sockaddr

module type UNIT = sig end
module Unit = struct end

module Int = Batteries.Int



(* Bah, Batteries v2 exports stdlib-incompatible variants of the basic
   I/O functions, we have to revert them. *)
open Batteries.Legacy

let open_out_bin = open_out_bin
let open_in_bin = open_in_bin
let close_out = close_out
let close_in = close_in
let output_string = output_string
let output = output
let stderr = stderr
let stdout = stdout
