(* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    *)
(* Copyright (C) 2007  Andr�s Var�n, Le Sy Vinh, Illya Bomash, Ward Wheeler,  *)
(* and the American Museum of Natural History.                                *)
(*                                                                            *)
(* This program is free software; you can redistribute it and/or modify       *)
(* it under the terms of the GNU General Public License as published by       *)
(* the Free Software Foundation; either version 2 of the License, or          *)
(* (at your option) any later version.                                        *)
(*                                                                            *)
(* This program is distributed in the hope that it will be useful,            *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(* GNU General Public License for more details.                               *)
(*                                                                            *)
(* You should have received a copy of the GNU General Public License          *)
(* along with this program; if not, write to the Free Software                *)
(* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301   *)
(* USA                                                                        *)

module Nodes = AllDirNode.AllDirF
module Edges = Edge.LazyEdge
module TreeOps = AllDirChar.F
module CharOps = AllDirChar.CharScripting

module Types = struct
    type a = Nodes.n
    type b = Edges.e
    type c = CharOps.cs
end

module DummyBatch = Batch.Dummy (Types)

module BatchM = 
    Scripting.Make (Types) (Nodes) (Edges) (TreeOps) (CharOps) (DummyBatch)

module LocalBatch = BatchLocal.Make (BatchM)

module M = 
    Scripting.Make (Types) (Nodes) (Edges) (TreeOps) (CharOps) (LocalBatch)

open M
include M

let welcome () =
    Status.user_message Status.Information Version.string

let initialize_seed () =
    let seed = truncate (Unix.time ()) in
    process_random_seed_set (empty ()) seed
