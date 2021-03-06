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

open PoyPrelude

let () = SadmanOutput.register "Hash_tbl" "$Revision: 1644 $"

(** hash_tbl.ml - contains a hash_tbl implementation based on 'A linear time
* majority tree algorithm' by N. Amenta, St. John etc. Reading that paper is
* needed to understand the workings of this hash table. This is not a general
* hash table and should not be used for any other purposes. *)

(** Indicates a double collision in the hash table. *)
exception Double_Collision 


(** The hash table record that contains the id generated by h2, the
 * number of times the partition occurs so far and the number of elements
 * in the partition. *)
type hash_tbl_record = { id : int ; 
                         (** identity generated by h2 *)
                         count : int ;
                         (** number of times this bi-partition occurs. *)
                         size : int ;
                         (** number of elements in the bi-partition. *) 
                         index : int
                         (** if the split was of size 1, then
                            this field holds the index of the leaf node.
                            since all legal indexes start at 0, this field
                            needs to be initialized to -1. *) }
                         
(** The array to hold the list of records. *)
type hash_tbl = { records : hash_tbl_record list array ;
                  (** the records of the hash table. *)
                  h1_params : int array ;
                  (** the a_i's for h1. *)
                  h2_params : int array ;
                  (** the a_i's for h2. *) 
                  m1 : int ;
                  (** prime number >= num_trees * num_nodes_per_tree *)
                  m2 : int 
                  (** prime number atleast twice as large as m1 *) }

(** An empty record. *)
let empty_record = { id = 0 ; 
                    count = 0 ; 
                    size = 0 ; 
                    index = -1 } 

(** [print_record recd] 
    @return (), prints the passed in record as a side-effect. *)
let print_record recd = 
    print_string ("{ id = " ^ (string_of_int recd.id) ^ 
                   " ; count = " ^ (string_of_int recd.count) ^
                   " ; size = " ^ (string_of_int recd.size) ^ 
                   " ; index = " ^ (string_of_int recd.index) ^ " }")

(** [make h1_p h2_p m1 m2]
    @param h1_p the parameters for the universal hash function h1.
    @param h2_p the parameters for the universal hash function h2.
    @param m1 a prime number greater than the number of trees *
        number_of_nodes_per_tree.
    @param m2 a prime number greater than m1. 
    @return a newly created hash table with n empty records and the given
        parameters. *)
let make h1_p h2_p m1 m2 = 
    (* make sure the parameters are consistent. *)
    assert((m1 > 0) && (m2 > m1)) ;
    assert((Array.length h1_p) = (Array.length h2_p)) ;
    for i = 0 to (Array.length h1_p) - 1 do
        assert((h1_p.(i) >= 0) && (h1_p.(i) < m1)) ;
        assert((h2_p.(i) >= 0) && (h2_p.(i) < m2)) ;
    done ;
    (* Make a hash table with empty records. *)
    { records = Array.make m1 [] ;
      h1_params = h1_p ;
      h2_params = h2_p ;
      m1 = m1 ;
      m2 = m2 }
      
(** [populate root_id (rtree, index_tbl, node_hashes) hash_tbl]
    @param root_id the root node that identifies the tree.
    @param rtree the rooted forest.
    @param index_tbl the index of the leaf nodes. 
    @param node_hashes create a hash table to store the node-hashes. 
     this table is need to efficiently compute the hash of an interior 
     node. 
    @return populates the hash table with the partitions of the tree. *)
let populate root_id (rtree, index_tbl, node_hashes) hash_tbl =         
    (* function to add the hash of the node to the hash table. *)
    let visit_op node_id hash_tbl =
        (* function to update the record list. *)
        let rec update_recd_lst recd recd_lst = 
            (* if the record list is empty. *)
            match recd_lst with
            (* add the record to the list. *)
            | [] -> [ recd ] 
            (* if the record list has records. insert sort into the list
            * based on the ids. *)
            | record :: records -> 
            begin
                let id1 = recd.id in
                let id2 = record.id in
                begin
                    match (compare id1 id2) with
                    | x when (x > 0) ->                
                        record :: (update_recd_lst recd records)
                    | x when (x < 0) ->
                            recd :: recd_lst
                    | _ -> (* x when (x = 0) *) 
                        (* if there was a double collision, exit. *)
                        if ( (recd.size != record.size) ||
                             (recd.index != record.index) ) then 
                            (raise Double_Collision)
                        else
                            (* update the record. *)
                            let record = { record with 
                                           count = record.count + 1 } in
                                record :: records 
                end
            end
        in
        match (Rtree.get_node node_id rtree) with
        (* if the node being visited is a leaf. *)
        | Rtree.Leaf(_) ->
            (* get the index of the node into the hash-table and
            * the hash of the bi-partition. *)
            let node_indx = (Hashtbl.find index_tbl node_id) in
            let node_hash = (hash_tbl.h1_params.(node_indx)) in
            let split_hash = (hash_tbl.h2_params.(node_indx)) in
            let _ = (Hashtbl.add node_hashes node_id 
                        (node_hash, split_hash, 1)) in
            let recd = { id = split_hash ;
                         count = 1 ;
                         size = 1 ;
                         (* assign the index. *)
                         index = node_indx } in
            (* check for the split in the hash table and update the 
            * record list. *)
            let recd_lst = hash_tbl.records.(node_hash) in
            (* update the list with the current split. *)
            let recd_lst = (update_recd_lst recd recd_lst) in
            begin
                hash_tbl.records.(node_hash) <- recd_lst ;
                hash_tbl
            end
        (* otherwise, if the node is an interior node or a root node. *)
        | Rtree.Interior(nd_id, _, l_id, r_id) 
        | Rtree.Root(nd_id, l_id, r_id) ->
            (* compute the node and split hashes. These are computed
            * from the hashes of the left and right children. Since,
            * we visit the tree in post-order, these values should
            * already be computed. *)
            let (lnh, lsh, lss) = Hashtbl.find node_hashes l_id in
            let (rnh, rsh, rss) = Hashtbl.find node_hashes r_id in
            let nh = (lnh + rnh) mod hash_tbl.m1 in
            let sh = (lsh + rsh) mod hash_tbl.m2 in
            let ss = lss + rss in
            let _ = Hashtbl.add node_hashes nd_id (nh, sh, ss) in
            (* leave the index of the record at -1 *)
            let recd = { empty_record with id = sh ;
                         count = 1 ;
                         size = ss } in
            (* check for the split in the hash table and update the 
            * record list. *)
            let recd_lst = hash_tbl.records.(nh) in
            (* update the list with the current split. *)
            let recd_lst = (update_recd_lst recd recd_lst) in
            begin
                hash_tbl.records.(nh) <- recd_lst ;
                hash_tbl
            end
    in
    let _ = 
        (Rtree.post_order_node_visitor visit_op root_id rtree hash_tbl) 
    in
        (node_hashes, hash_tbl) 
                                                  
(** [get_data (nd_hash, sp_hash) hash_tbl]
   @return the hash table record associated with the node. *)
let get_data (nd_hash, sp_hash) hash_tbl = 
    let recd_lst = hash_tbl.records.(nd_hash) in
    let find_fn recd = (recd.id = sp_hash) in
    let recd = (List.find find_fn recd_lst) in
        (recd.count, recd.size, recd.index) 



(** {2 Interface} *)

(** [Interface] provides a more reasonable interface to the functions in this
    module. *)
module Interface = struct
    (** [fp] is an individual fingerprint *)
    type fp = (int * int)
            
    (** [t] is an object that responds to queries for the hash values on a tree
    *)
    type t = (int, int * int * int) Hashtbl.t * hash_tbl

    (** [def_safety] is the default safety level (see paper, above) *)
    let def_safety = 500

    (** [make_params ?safety_factor ntrees nodes_per_tree] makes an
        initialization parameter for a certain number of trees and nodes per
        tree.  These parameters can be used in [make_wparams], below.  The
        advantage (compared to using [make]) is being able to make several hash
        tables with the same parameters, so that they end up compatible.
        Otherwise, fingerprints mean different things in different hash
        tables. *)
    let make_params ?(safety_factor = def_safety) ntrees nodes_per_tree =
        (* m1 is prime > ntrees * nodes_per_tree *)
        let m1 = Primes.Probable.next_prime_gt (ntrees * nodes_per_tree) in
        (* m2 is prime > m1 *)
        let m2 = Primes.Probable.next_prime_gt (m1 * safety_factor) in

        let num_leaves = (nodes_per_tree + 1) / 2 in

        let h1_p = Array.make num_leaves 0 in
        let h2_p = Array.make num_leaves 0 in
        for i = 0 to num_leaves - 1 do
            h1_p.(i) <- (Random.int m1) ;
            h2_p.(i) <- (Random.int m2)
        done;

        (m1, m2, h1_p, h2_p, nodes_per_tree)

    (** [make_wparams (m1, m2, h1_p, h2_p, nodes_per_tree)] makes a hash table
        using the parameters made above. *)
    let make_wparams (m1, m2, h1_p, h2_p, nodes_per_tree) =
        let hash_tbl = make h1_p h2_p m1 m2 in
        let ht = Hashtbl.create nodes_per_tree in
        ((ht, hash_tbl) : t)

    (** [make] is a combination of [make_params] and [make_wparams], above. *)
    let make ?(safety_factor = def_safety) ntrees nodes_per_tree =
        (* m1 is prime > ntrees * nodes_per_tree *)
        let m1 = Primes.Probable.next_prime_gt (ntrees * nodes_per_tree) in
        (* m2 is prime > m1 *)
        let m2 = Primes.Probable.next_prime_gt (m1 * safety_factor) in

        let num_leaves = (nodes_per_tree + 1) / 2 in

        let h1_p = Array.make num_leaves 0 in
        let h2_p = Array.make num_leaves 0 in
        for i = 0 to num_leaves - 1 do
            h1_p.(i) <- (Random.int m1) ;
            h2_p.(i) <- (Random.int m2)
        done;

        let hash_tbl = make h1_p h2_p m1 m2 in
        let ht = Hashtbl.create nodes_per_tree in
        ((ht, hash_tbl) : t)

    (** [populate t tree rootid] populats a hash table [t] with the tree with
        root [rootid] from [tree] *)
    let populate (t : t) tree rootid =
        let (node_hashes, hash_tbl) = t in

        let num_nodes = Rtree.get_num_nodes rootid tree in
        let num_leaves = (num_nodes + 1) / 2 in
        let index_node_table, node_index_table =
            Rtree.index_nodes (0, num_leaves) num_nodes rootid tree in
        let node_hashes', hash_tbl' =
            populate rootid (tree, node_index_table, node_hashes) hash_tbl in
        ((node_hashes', hash_tbl') : t)

    (** [query t nodeid] returns the hash of the clade below [nodeid] from [t] *)        
    let query (t : t) nodeid =
        let (node_hashes, hash_tbl) = t in

        let (h1, h2, _) = Hashtbl.find node_hashes nodeid in
        ((h1, h2) : fp)
end
