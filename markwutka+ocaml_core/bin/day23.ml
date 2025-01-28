(* I guessed wrong that part B might involve longer cycles,
   so part A is more complicated than it needed to be since
   I wrote it to find cycles of an arbitrary length.

   For part B, if you don't check that each node can connect
   directly to all the members of the group, you'll find that
   all nodes can eventually reach all other nodes.
*)

open Core
open Advent_lib

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)
    
let add_to_adj map pair =
  let update v adj_opt =
    match adj_opt with
    | None -> Set.add StringSet.empty v
    | Some adj -> Set.add adj v
  in
  let x1 = List.hd_exn pair in
  let x2 = List.nth_exn pair 1 in
  Map.update (Map.update map x2 ~f:(update x1)) x1 ~f:(update x2) 

let merge_pairs set pair =
  Set.add (Set.add set (List.nth_exn pair 1)) (List.hd_exn pair) 
    
let get_cycles sep adj_map n cycles from =
  let make_string s = String.concat ~sep:sep (Set.to_list s) in
  let rec add_adj n cycle cycles node =
    if n = 1 then
      if Set.mem (Map.find_exn adj_map node) from then
        Set.add cycles (make_string (Set.add cycle node)) 
      else
        cycles
    else
      let next_nodes = Map.find_exn adj_map node in
      let next_cycle = Set.add cycle node in
      let to_add = Set.diff next_nodes next_cycle in
      List.fold ~f:(add_adj (n-1) next_cycle) ~init:cycles
        (Set.to_list to_add)
  in
  add_adj n StringSet.empty cycles from
      
let has_t_start str =
  let rec loop pos =
    if pos >= String.length str then
      false
    else if Char.(str.[pos] = 't') then
      true
    else
      loop (pos+2)
  in
  loop 0

let get_groups adj_map node_list =
  let rec is_in_group node acc =
    match acc with
    | [] -> false
    | s :: rest -> if Set.mem s node then true
      else is_in_group node rest
  in
  let rec connects_to_group group_list node =
    match group_list with
    | [] -> true
    | group_node :: rest ->
      if not (Set.mem (Map.find_exn adj_map node) group_node) then
        false
      else
        connects_to_group rest node
  in
  let rec add_to_group group node =
    if connects_to_group (Set.to_list group) node then
      let new_group = Set.add group node in
      let adj = Map.find_exn adj_map node in
      let to_add = Set.diff adj new_group in
      List.fold ~f:add_to_group ~init:new_group
        (Set.to_list to_add)
    else
      group
  in
  let rec make_group nodes acc =
    match nodes with
    | [] -> acc
    | node :: rest ->
      if is_in_group node acc then
        make_group rest acc
      else
        make_group rest ((add_to_group StringSet.empty node)::acc)
  in
  make_group node_list []

let rec max_group (max_val,max_str) groups =
  match groups with
  | [] -> max_str
  | group :: rest ->
    if (Set.length group) > max_val then
      max_group (Set.length group,
                 String.concat ~sep:"," (Set.to_list group)) rest
    else
      max_group (max_val, max_str) rest

let day23 () =
  let lines = Mwlib.read_file "data/day23.txt" in
  let pairs = List.map ~f:(String.split ~on:'-') lines in
  let adj_map = List.fold ~f:add_to_adj ~init:StringMap.empty pairs in
  let node_list = Set.to_list
      (List.fold ~f:merge_pairs ~init:StringSet.empty pairs) in
  let three_cycles = List.fold ~f:(get_cycles "" adj_map 3)
      ~init:StringSet.empty node_list in
  let chief_cycles = List.filter ~f:has_t_start (Set.to_list three_cycles) in
  let resulta = List.length chief_cycles in
  let groups = get_groups adj_map node_list in
  let resultb = max_group (0,"") groups in
  Printf.printf "day23a = %d\nday23b = %s\n" resulta resultb;;
 
day23 ();;
