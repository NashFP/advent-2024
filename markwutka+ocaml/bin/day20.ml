(*
  There's not much trickery to this. I originally kepy
  keep an array whose elements were either 0 meaning the
  cell isn't on the path, or the cell's distance along
  the path. Then I realized that since the path never
  intersects itself, I can figure out the next item
  on the path by keeping track of the previous position
  and making sure I don't go backwards. That allowed
  me to store the distance along the path in the path
  itself.

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

let make_path lines =
  let (sx,sy) = find_start lines in
  let find_next (x,y) (prev_x,prev_y) =
    let is_next (x,y) =
      let ch = lines.(y).[x] in
      (ch == '.' || ch == 'E') && (x != prev_x || y != prev_y) in
    
    (* Try each direction, only one should succeed *)
    if is_next (x-1,y) then (x-1,y)
    else if is_next (x,y-1) then (x,y-1)
    else if is_next (x+1,y) then (x+1,y)
    else (x,y+1)
  in
  let rec loop pos prev_pos path len =
    let (x,y) = find_next pos prev_pos in
    let next_path = (x,y,len) :: path in
    if lines.(y).[x] == 'E' then
      List.rev next_path
    else
      loop (x,y) pos next_path (len+1)
  in
  loop (sx,sy) (-1,-1) [(sx,sy,1)] 2

let cheat_list max_pico min_cheat path =
  let grid_dist (x0,y0) (x1,y1) =
    abs (x0-x1) + abs (y0-y1) in
  let try_add_cheats (fx,fy,curr_val) cheats (dx,dy,cheat_val) =
    let dist = grid_dist (fx,fy) (dx,dy) in
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
  let path = make_path lines in
  let resulta = cheat_list 2 100 path in
  let resultb = cheat_list 20 100 path in
  Printf.printf "day20a = %d\nday20b = %d\n" resulta resultb;;
  
day20 ();;

