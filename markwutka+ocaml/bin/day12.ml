(*
  The basic flow of day 12 is:
  1. Read the file and turn it into a 2-d matrix
  2. Find all the groups of adjacent squares
     Keep an initially-empty set of (x,y) indicating that a square
     has been visited. For each coordinate, if it hasn't been visited,
     then mark it visited and start a new group, and try to add its
     adjacent squares.
  3. Compute the perimeter. For each group, look at each square, and
     add 1 to the perimeter length for each side of the square that faces
     a different square or the edge of the map.
  4. Compute the number of sides. For each group, look at each square and
     see if any of its corners are corners of the region - either an outer
     corner where you look for different/invalid squares in two adjacent
     directions, or an inner corner where you look for valid & same squares
     in two adjacent directions, and then a different/invalid square diagonally
     adjacent between the other two.
 *)
      
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
    (* If this square is valid and unvisited, add it and visit its adjacent squares *)
    if is_valid (x,y) && is_unvisited visited (x,y) &&
         grid.(x).(y) == ch then
      let v0 = PairsSet.add (x,y) visited in
      let g0 = (x,y) :: group in
      List.fold_left (try_visit ch) (v0, g0) [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
    else
      (* Otherwise if the square is invalid or visited, don't do anything *)
      (visited, group) in
  let make_group (visited, groups) (coord,ch) =
    let (v_new, group) = try_visit ch (visited, []) coord in
    (v_new, group :: groups) in
  snd (List.fold_left make_group (PairsSet.empty, []) (Mwlib.matrix_to_list grid))

let get_group_value grid group =
  let (width, height) = Mwlib.matrix_dim grid in
  let get_perimeter_value (x,y) = 
    let is_valid (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
    let ch = grid.(x).(y) in
    let perimeter_edge (x,y) =
      (* If the adjacent square is invalid or in a different group,
         count that side as part of the perimeter *)
      if not (is_valid (x,y)) || ch != grid.(x).(y) then 1 else 0
    in
    (* Sum the edge values for all four sides of the square *)
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
    (* For an outer corner, look for invalid squares in two directions:
         X   X
        Xo   oX  oX    Xo
                 X      X
     *)         
    let outer_corner_value ((x1,y1),(x2,y2)) =
      if (not (is_valid (x1,y1)) || ch != grid.(x1).(y1)) &&
           (not (is_valid (x2,y2)) || ch != grid.(x2).(y2)) then 1 else 0
    in
    (* For an inner corner, look for valid adjacent squares in two directions
       and an invalid one diagonally across:
       XO    OX
       Oo    oO     oO     Oo
                    OX     XO
     *)
    let inner_corner_value ((x1,y1),(x2,y2),(x3,y3)) =
      if (is_valid (x1,y1) && is_valid (x2,y2) &&
            ch == grid.(x1).(y1) && ch == grid.(x2).(y2)) &&
           (not (is_valid (x3,y3)) || ch != grid.(x3).(y3)) then 1 else 0
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
