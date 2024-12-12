open Advent_lib
open Option

module IntPairs =
  struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Stdlib.compare x0 x1 with
             0 -> Stdlib.compare y0 y1
             | c -> c
       end

module PairsMap = Map.Make(IntPairs)
module PairsSet = Set.Make(IntPairs) 

let make_grid lines =
  let width = String.length (List.hd lines) in
  let height = List.length lines in
  let add_elem row grid ch col = PairsMap.add (col,row) ch grid in
  let add_row grid row row_num =
    List.fold_left2 (add_elem row_num) grid (Mwlib.string_to_list row)
      (Mwlib.range 0 width) in  
  (width, height,
   List.fold_left2 add_row PairsMap.empty lines (Mwlib.range 0 height))

let get_groups width height grid =
  let is_valid (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
  let is_unvisited visited pos = not (PairsSet.mem pos visited) in
  let rec try_visit ch (visited, group) (x,y) =
    if is_valid (x,y) && is_unvisited visited (x,y) &&
         (PairsMap.find (x,y) grid) == ch then
      let v0 = PairsSet.add (x,y) visited in
      let g0 = (x,y) :: group in
      List.fold_left (try_visit ch) (v0, g0) [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    else
      (visited, group) in
  let rec make_group (visited, groups) (coord,ch) =
    let (v_new, group) = try_visit ch (visited, []) coord in
    if List.is_empty group then
      (visited, groups)
    else
      (v_new, group :: groups) in
  snd (List.fold_left make_group (PairsSet.empty, []) (PairsMap.to_list grid))

let get_perimeter_value width height grid (x,y) = 
  let is_valid (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
  let ch = PairsMap.find (x,y) grid in
  let perimeter_edge (x,y) =
    if not (is_valid (x,y)) || ch != PairsMap.find (x,y) grid then
      1
    else
      0
  in
  List.fold_left (+) 0 (List.map perimeter_edge [(x-1,y);(x+1,y);(x,y-1);x,y+1])

let get_group_perimeter width height grid group =
  List.fold_left (+) 0 (List.map (get_perimeter_value width height grid) group)

let get_group_value width height grid group =
  (List.length group) * (get_group_perimeter width height grid group)

let get_corner_value width height grid (x,y) = 
  let is_valid (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
  let ch = PairsMap.find (x,y) grid in
  let outer_corner_value ((x1,y1),(x2,y2)) =
    if (not (is_valid (x1,y1)) || ch != PairsMap.find (x1,y1) grid) &&
         (not (is_valid (x2,y2)) || ch != PairsMap.find (x2,y2) grid) then
      1
    else
      0
  in
  let inner_corner_value ((x1,y1),(x2,y2),(x3,y3)) =
    if (is_valid (x1,y1) && is_valid (x2,y2) &&
          ch == PairsMap.find (x1,y1) grid &&
            ch == PairsMap.find (x2,y2) grid) &&
         (not (is_valid (x3,y3)) || ch != PairsMap.find (x3,y3) grid) then
      1
    else
      0
  in
  (List.fold_left (+) 0 (List.map outer_corner_value [((x-1,y),(x,y-1));
                                                      ((x,y-1),(x+1,y));
                                                      ((x+1,y),(x,y+1));
                                                      ((x,y+1),(x-1,y))])) +
    (List.fold_left (+) 0 (List.map inner_corner_value
                             [((x-1,y),(x,y-1),(x-1,y-1));
                              ((x,y-1),(x+1,y),(x+1,y-1));
                              ((x+1,y),(x,y+1),(x+1,y+1));
                              ((x,y+1),(x-1,y),(x-1,y+1))]))

let get_group_corners width height grid group =
    List.fold_left (+) 0 (List.map (get_corner_value width height grid) group)

let get_group_value_b width height grid group =
  (List.length group) * (get_group_corners width height grid group)
                        
let day12 () =
  let lines = Mwlib.read_file "data/day12.txt" in
  let (width, height, grid) = make_grid lines in
  let groups = get_groups width height grid in
  let resulta = List.fold_left (+) 0
                  (List.map (get_group_value width height grid) groups) in
  let resultb = List.fold_left (+) 0
                  (List.map (get_group_value_b width height grid) groups) in
  Printf.printf "day12a = %d\nday12b = %d\n" resulta resultb;;
