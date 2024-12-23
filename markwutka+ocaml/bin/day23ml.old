open Advent_lib

module StringKey =
  struct
    type t = string
    let compare = String.compare
  end

module StringSet = Set.Make(StringKey)
module StringMap = Map.Make(StringKey)

let add_set_to_map map set =
  let add_set_member map set member =
    if not (StringSet.mem member map) then
      StringMap.add member set map
    else
      let new_set = StringSet.union set (StringMap.find member map) in
      if (StringSet.cardinal new_set) != (StringSet.cardinal set) then
        List.fold_left add_set_to_map map new_set (StringSet.to_list new_set)
      else
        map
  in
  List.fold_left (add_set_member map set) (StringSet.to_list set)
  

let day23 () =
  let lines = Mwlib.read_file "data/day23test.txt" in
  let pairs = List.map StringSet.of_list
                (List.map (String.split_on_char '-') lines) in
  let set_map = List.fold_left add_pairs_to_map StringMap.empty pairs in
  segment_map
 

