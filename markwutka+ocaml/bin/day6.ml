open Advent_lib

exception Err of string

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

let find_guard grid =
  let rec loop y =
    if y >= Array.length grid then
      raise (Err "Can't find guard")
    else
      let row = Array.get grid y in
      if String.contains row '^' then
        ((String.index row '^', y), Up)
      else if String.contains row '>' then
        ((String.index row '>', y), Right)
      else if String.contains row 'v' then
        ((String.index row 'v', y), Down)
      else if String.contains row '<' then
        ((String.index row '<', y), Left)
      else
        loop (y+1)
  in
  loop 0

let make_map_entry grid y x =
  if grid.(y).[x] == '#' then
    16
  else
    0

let move_guard positions guard =
  let get_pos (x,y) = positions.(y).(x) in
  let add_dir (x,y) dir = (positions.(y).(x) <-
                          positions.(y).(x) lor (dir_bit dir)) in
  let is_obstructed pos = get_pos pos == 16 in
  let is_invalid (x,y) =
    x < 0 || x >= Array.length positions.(0) || y < 0 || y >= Array.length positions in
  let mark_position pos dir =
    let is_unique = if get_pos pos == 0 then 1 else 0 in
    (add_dir pos dir; is_unique) in
  let been_here (x,y) dir =
    (get_pos (x,y) land (dir_bit dir)) != 0 in
  let rec loop (pos,dir) unique =
    if been_here pos dir then
      (0, true)
    else
      let next_unique = unique + mark_position pos dir in 
      let next_pos = move_dir pos dir in
      if is_invalid next_pos then
        (next_unique, false)
      else if is_obstructed next_pos then
        let next_dir = turn_right dir in
        loop (pos, next_dir) next_unique
      else
        loop (next_pos, dir) next_unique
  in
  loop guard 0

let try_obstruction grid guard (x,y) =
  let is_guard_pos ((gx,gy),_) = gx == x && gy == y in
  let positions = Array.init_matrix (Array.length grid)
                    (String.length grid.(0)) (make_map_entry grid) in
  let v = positions.(y).(x) in
  if v == 16 || is_guard_pos guard then
    false
  else
    (positions.(y).(x) <- 16;
     let (_, result) = move_guard positions guard in
     result)
     
let day6 () =
  let lines = Mwlib.read_file "data/day6.txt" in
  let grid = Array.of_list lines in
  let positions = Array.init_matrix (Array.length grid)
                    (String.length grid.(0)) (make_map_entry grid) in
  let guard = find_guard grid in
  let (resulta,_) = move_guard positions guard in
  let all_positions = Mwlib.product (Mwlib.range 0 (String.length grid.(0)))
                        (Mwlib.range 0 (Array.length grid)) in
  let resultb = List.length (List.filter (try_obstruction grid guard) all_positions) in
  Printf.printf "day6a = %d\nday6b = %d\n" resulta resultb;;

day6 ();;
