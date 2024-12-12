open Advent_lib

module IntPairs =
  struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Stdlib.compare x0 x1 with
             0 -> Stdlib.compare y0 y1
             | c -> c
       end

module PairsSet = Set.Make(IntPairs) 

let make_grid lines =
  let width = String.length lines.(0) in
  let height = Array.length lines in
  let get_item x y = lines.(y).[x] in
  Array.init_matrix width height get_item

let get_groups grid =
  let (width,height) = Mwlib.matrix_dim grid in
  let is_valid (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
  let is_unvisited visited pos = not (PairsSet.mem pos visited) in
  let rec try_visit ch (visited, group) (x,y) =
    if is_valid (x,y) && is_unvisited visited (x,y) &&
         grid.(x).(y) == ch then
      let v0 = PairsSet.add (x,y) visited in
      let g0 = (x,y) :: group in
      List.fold_left (try_visit ch) (v0, g0) [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    else
      (visited, group) in
  let make_group (visited, groups) (coord,ch) =
    let (v_new, group) = try_visit ch (visited, []) coord in
    if List.is_empty group then
      (visited, groups)
    else
      (v_new, group :: groups) in
  snd (List.fold_left make_group (PairsSet.empty, []) (Mwlib.matrix_to_list grid))

let get_group_value grid group =
  let (width, height) = Mwlib.matrix_dim grid in
  let get_perimeter_value (x,y) = 
    let is_valid (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
    let ch = grid.(x).(y) in
    let perimeter_edge (x,y) =
      if not (is_valid (x,y)) || ch != grid.(x).(y) then
        1
      else
        0
    in
    List.fold_left (+) 0 (List.map perimeter_edge [(x-1,y);(x+1,y);(x,y-1);x,y+1])
  in
  let get_group_perimeter group =
    List.fold_left (+) 0 (List.map get_perimeter_value group) in
  (List.length group) * (get_group_perimeter group)

let get_group_value_b grid group =
  let (width,height) = Mwlib.matrix_dim grid in
  let get_corner_value (x,y) = 
    let is_valid (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
    let ch = grid.(x).(y) in
    let outer_corner_value ((x1,y1),(x2,y2)) =
      if (not (is_valid (x1,y1)) || ch != grid.(x1).(y1)) &&
           (not (is_valid (x2,y2)) || ch != grid.(x2).(y2)) then
        1
      else
        0
    in
    let inner_corner_value ((x1,y1),(x2,y2),(x3,y3)) =
      if (is_valid (x1,y1) && is_valid (x2,y2) &&
            ch == grid.(x1).(y1) && ch == grid.(x2).(y2)) &&
           (not (is_valid (x3,y3)) || ch != grid.(x3).(y3)) then
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
                                ((x,y+1),(x-1,y),(x-1,y+1))])) in
  let get_group_corners group =
    List.fold_left (+) 0 (List.map get_corner_value group)
  in
  (List.length group) * (get_group_corners group)
                        
let day12 () =
  let lines = Mwlib.read_file "data/day12.txt" in
  let grid = make_grid (Array.of_list lines) in
  let groups = get_groups grid in
  let resulta = List.fold_left (+) 0
                  (List.map (get_group_value grid) groups) in
  let resultb = List.fold_left (+) 0
                  (List.map (get_group_value_b grid) groups) in
  Printf.printf "day12a = %d\nday12b = %d\n" resulta resultb;;

day12 ();;
