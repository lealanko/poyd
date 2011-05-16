(* POY 4.0 Beta. A phylogenetic analysis program using Dynamic Homologies.    *)
(* Copyright (C) 2007  Andrés Varón, Le Sy Vinh, Illya Bomash, Ward Wheeler,  *)
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

let () = SadmanOutput.register "Main" "$Revision: 2853 $"

(* $Id: main.ml 2853 2008-05-18 14:21:15Z andres $ *)

let seed = truncate (Unix.time ())

let is_running_alone = ref false

IFDEF USEPARALLEL THEN
let my_rank = Mpi.comm_rank Mpi.comm_world

let () = 
    if my_rank <> 0 then SadmanOutput.do_sadman := false
    else ()

ELSE

let args =
    (* TODO: Fix the arguments preprocessing *)
    Sys.argv

END

let () = SadmanOutput.register "Main" "$Revision: 2853 $"

let () = Status.init ()

let () =
    try
        Arg.parse_argv Phylo.args Arguments.parse_list Arguments.anon_fun 
        Arguments.usage 
    with
    | Arg.Help _ ->
            Arg.usage Arguments.parse_list Arguments.usage;
            exit 0
    | Arg.Bad x ->
            prerr_string ("Bad argument: " ^ x);
            Arg.usage Arguments.parse_list Arguments.usage;
            exit 1

let () = MainUtil.welcome_msg ()

let regen_and_save filename codestring regenfn =
    let data = regenfn () in
    let outfile = open_out_bin filename in
    Marshal.to_channel outfile (codestring, data) [];
    close_out outfile;
    data
let unmarshal_or_regen filename codestring regenfn =
    try
        let infile = open_in_bin filename in
        let (code, a) = Marshal.from_channel infile in
        close_in infile;
        if code = codestring
        then a
        else regen_and_save filename codestring regenfn
    with Sys_error _ ->
        regen_and_save filename codestring regenfn

(* First load the input *)
let script = match MainUtil.load_script !Arguments.input with
    | None when !Arguments.just_exit -> exit 1
    | script -> script

(*** Begin output. *)
let () = MainUtil.begin_sadman ()

let _ = Phylo.initialize_seed ()

IFDEF USEPARALLEL THEN
let _ =
    let tsize = Mpi.comm_size Mpi.comm_world in
    if my_rank = 0 && tsize > 1 then
        Status.user_message Status.Information 
        ("Running in parallel with " ^ string_of_int (Mpi.comm_size
        Mpi.comm_world) ^ " processes")
    else if tsize = 1 then
        Status.user_message Status.Information
        "Running sequentially."
    else ();
    let arr = Array.init tsize (fun x -> seed + x) in
    let seed = Mpi.scatter_int arr 0 Mpi.comm_world in
    Phylo.process_random_seed_set (Phylo.empty ()) seed
END

let run command = 
    let res = 
        Phylo.run 
            ~output_file:(!(Arguments.dump_file)) 
            ~start:(Phylo.get_console_run ()) command 
    in
    Phylo.set_console_run res


let retcode = MainUtil.main script run !Arguments.just_exit
let _ = exit retcode
