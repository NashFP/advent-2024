(*
  There's not much trickery to this. I keep an array
  whose elements are either 0 meaning the cell isn't
  on the path, or the cell's distance along the path.

  When looking for the cheats, a cheat can actually
  cross an existing part of the path, so you can just
  use Manhattan distance for the distance between any
  two points along the path.
 *)

open Advent_lib

let find_start lines =
  let width = String.length lines.(0) in
  let height = Array.length lines in
  let is_start (x,y) = lines.(y).[x] == 'S' in
  List.hd (List.filter is_start
             (Mwlib.product (Mwlib.range 0 width)
                (Mwlib.range 0 height)))

let make_grid lines =
  let width = String.length lines.(0) in
  let height = Array.length lines in
  let (sx,sy) = find_start lines in
  let array_init x y = if x == sx && y == sy then 1 else 0 in
  let new_grid = Array.init_matrix width height array_init in  
  let find_next (x,y) =
    let is_next (x,y) =
      (lines.(y).[x] == '.' || lines.(y).[x] == 'E') &&
        new_grid.(x).(y) == 0 in
    if is_next (x-1,y) then (x-1,y)
    else if is_next (x,y-1) then (x,y-1)
    else if is_next (x+1,y) then (x+1,y)
    else (x,y+1)
  in
  let rec loop pos path len =
    let (x,y) = find_next pos in
    new_grid.(x).(y) <- len;
    let next_path = (x,y) :: path in
    if lines.(y).[x] == 'E' then
      (List.rev next_path, new_grid)
    else
      loop (x,y) next_path (len+1)
  in
  loop (sx,sy) [(sx,sy)] 2

let cheat_list max_pico min_cheat grid path =
  let grid_dist (x0,y0) (x1,y1) =
    abs (x0-x1) + abs (y0-y1) in
  let try_add_cheats (fx,fy) cheats (dx,dy) =
    let dist = grid_dist (fx,fy) (dx,dy) in
    let cheat_val = grid.(dx).(dy) in
    let curr_val = grid.(fx).(fy) in 
    if dist <= max_pico && cheat_val > curr_val + dist &&
         (cheat_val - (curr_val + dist)) >= min_cheat then
      cheats + 1
    else
      cheats   
  in
  let rec loop curr_path cheats =
    match curr_path with
    | [] -> cheats
    | from_pos :: rest ->
       loop rest (List.fold_left (try_add_cheats from_pos) cheats rest)
  in
  loop path 0
       

let day20 () =
  let lines = Array.of_list (Mwlib.read_file "data/day20.txt") in
  let (path,grid) = make_grid lines in
  let resulta = cheat_list 2 100 grid path in
  let resultb = cheat_list 20 100 grid path in
  Printf.printf "day20a = %d\nday20b = %d\n" resulta resultb;;
  
day20 ();;

