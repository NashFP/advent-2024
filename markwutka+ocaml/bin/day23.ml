(* I guessed wrong that part B might involve longer cycles,
   so part A is more complicated than it needed to be since
   I wrote it to find cycles of an arbitrary length.

   For part B, if you don't check that each node can connect
   directly to all the members of the group, you'll find that
   all nodes can eventually reach all other nodes.
*)

open Advent_lib

module StringKey =
  struct
    type t = string
    let compare = String.compare
  end

module StringIntKey =
struct
  type t = string * int
  let compare (s0,i0) (s1,i1) =
    match String.compare s0 s1 with
    | 0 -> Stdlib.compare i0 i1
    | c -> c
end

module StringSet = Set.Make(StringKey)
module StringMap = Map.Make(StringKey)
module StringIntMap = Map.Make(StringIntKey)
    
let add_to_adj map pair =
  let update v adj_opt =
    match adj_opt with
    | None -> Some (StringSet.add v StringSet.empty)
    | Some adj -> Some (StringSet.add v adj)
  in
  let x1 = List.hd pair in
  let x2 = List.nth pair 1 in
  StringMap.update x1 (update x2) (StringMap.update x2 (update x1) map)

let merge_pairs set pair =
  StringSet.add (List.hd pair) (StringSet.add (List.nth pair 1) set)
    
let get_cycles sep adj_map n cycles from =
  let make_string s = String.concat sep (StringSet.to_list s) in
  let rec add_adj n cycle cycles node =
    if n == 1 then
      if StringSet.mem from (StringMap.find node adj_map) then
        StringSet.add (make_string (StringSet.add node cycle)) cycles
      else
        cycles
    else
      let next_nodes = StringMap.find node adj_map in
      let next_cycle = StringSet.add node cycle in
      let to_add = StringSet.diff next_nodes next_cycle in
      List.fold_left (add_adj (n-1) next_cycle) cycles
        (StringSet.to_list to_add)
  in
  add_adj n StringSet.empty cycles from
      
let has_t_start str =
  let rec loop pos =
    if pos >= String.length str then
      false
    else if str.[pos] == 't' then
      true
    else
      loop (pos+2)
  in
  loop 0

let get_groups adj_map node_list =
  let rec is_in_group node acc =
    match acc with
    | [] -> false
    | s :: rest -> if StringSet.mem node s then true
      else is_in_group node rest
  in
  let rec connects_to_group group_list node =
    match group_list with
    | [] -> true
    | group_node :: rest ->
      if not (StringSet.mem group_node (StringMap.find node adj_map)) then
        false
      else
        connects_to_group rest node
  in
  let rec add_to_group group node =
    if connects_to_group (StringSet.to_list group) node then
      let new_group = StringSet.add node group in
      let adj = StringMap.find node adj_map in
      let to_add = StringSet.diff adj new_group in
      List.fold_left add_to_group new_group
        (StringSet.to_list to_add)
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
    if (StringSet.cardinal group) > max_val then
      max_group (StringSet.cardinal group,
                 String.concat "," (StringSet.to_list group)) rest
    else
      max_group (max_val, max_str) rest

let day23 () =
  let lines = Mwlib.read_file "data/day23.txt" in
  let pairs = List.map (String.split_on_char '-') lines in
  let adj_map = List.fold_left add_to_adj StringMap.empty pairs in
  let node_list = StringSet.to_list
      (List.fold_left merge_pairs StringSet.empty pairs) in
  let three_cycles = List.fold_left (get_cycles "" adj_map 3)
      StringSet.empty node_list in
  let chief_cycles = List.filter has_t_start (StringSet.to_list three_cycles) in
  let resulta = List.length chief_cycles in
  let groups = get_groups adj_map node_list in
  let resultb = max_group (0,"") groups in
  Printf.printf "day23a = %d\nday23b = %s\n" resulta resultb;;
 

