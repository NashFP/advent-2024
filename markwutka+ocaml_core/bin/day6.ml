open Core
open Advent_lib

exception Err of string

type pos_map = PosMap of (int array array) * (int * int) list * int

type dir_type = Up | Right | Down | Left

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let dir_bit = function
  | Up -> 1
  | Right -> 2
  | Down -> 4
  | Left -> 8

let move_dir (x, y) dir =
  match dir with
  | Up -> (x, y-1)
  | Right -> (x+1, y)
  | Down -> (x, y+1)
  | Left -> (x-1, y)

let visit_map (PosMap (positions, mod_pos, unique)) (x,y) dir =
  let v = positions.(y).(x) in
  let new_unique = if v = 0 then unique + 1 else unique in
  if (v land (dir_bit dir)) <> 0 then
    PosMap (positions, mod_pos, new_unique)
  else
    (positions.(y).(x) <- positions.(y).(x) lor (dir_bit dir);
     PosMap (positions, ((x,y) :: mod_pos), new_unique))

let add_obstruction (PosMap (positions, mod_pos, unique)) (x,y) =
  positions.(y).(x) <- 16;
  PosMap (positions, (x,y)::mod_pos, unique)

let clear_map (PosMap (positions, mod_pos, _)) =
  let rec loop = function
    | [] -> PosMap (positions, [], 0)
    | (x,y) :: rest -> (positions.(y).(x) <- 0; loop rest)
  in
  loop mod_pos

let get_map (PosMap (positions, _, _)) (x,y) =
  positions.(y).(x)

let is_invalid (PosMap (positions, _, _)) (x,y) =
  x < 0 || x >= Array.length positions.(0) ||
    y < 0 || y >= Array.length positions

let is_obstructed (PosMap (positions, _, _)) (x,y) =
  positions.(y).(x) = 16

let get_unique (PosMap (_, _, unique)) = unique

let find_guard grid =
  let rec loop y =
    if y >= Array.length grid then
      raise (Err "Can't find guard")
    else
      let row = Array.get grid y in
      if String.contains row '^' then
        ((String.index_exn row '^', y), Up)
      else if String.contains row '>' then
        ((String.index_exn row '>', y), Right)
      else if String.contains row 'v' then
        ((String.index_exn row 'v', y), Down)
      else if String.contains row '<' then
        ((String.index_exn row '<', y), Left)
      else
        loop (y+1)
  in
  loop 0

let make_map_entry grid y x =
  if Char.equal grid.(y).[x] '#' then
    16
  else
    0

let move_guard pos_map guard =
  let been_here (x,y) dir =
    (get_map pos_map (x,y) land (dir_bit dir)) <> 0 in
  let rec loop (pos,dir) pos_map =
    if been_here pos dir then
       (0, true, clear_map pos_map)
    else
      let next_map = visit_map pos_map pos dir in
      let next_pos = move_dir pos dir in
      if is_invalid pos_map next_pos then
         (get_unique next_map, false, clear_map next_map)
      else if is_obstructed pos_map next_pos then
        let next_dir = turn_right dir in
        loop (pos, next_dir) next_map
      else
        loop (next_pos, dir) next_map
  in
  loop guard pos_map

let try_obstruction pos_map guard (x,y) =
  let is_guard_pos ((gx,gy),_) = gx = x && gy = y in
  let v = get_map pos_map (x,y) in
  if v = 16 || is_guard_pos guard then
    false
  else
     let new_map = add_obstruction pos_map (x,y) in
     let (_, result, _) = move_guard new_map guard in
     let _ = clear_map new_map in
     result
     
let day6 () =
  let lines = Mwlib.read_file "data/day6.txt" in
  let grid = Array.of_list lines in
  let positions = Mwlib.init_matrix (Array.length grid)
                    (String.length grid.(0)) ~f:(make_map_entry grid) in
  let guard = find_guard grid in
  let pos_map = PosMap (positions, [], 0) in
  let (resulta,_,next_map) = move_guard pos_map guard in
  let all_positions = List.cartesian_product
      (List.range 0 (String.length grid.(0)))
                        (List.range 0 (Array.length grid)) in
  let resultb = List.length (List.filter ~f:(try_obstruction next_map guard)
                               all_positions) in
  Printf.printf "day6a = %d\nday6b = %d\n" resulta resultb;;

day6 ();;
