(* Part A was pretty easy, just multiplying the robot's
   move delta by the number of seconds and doing a modulo
   to keep it in range (I had to make a modulo func that always
   returned a positive number).

   For part B, I started off looking at manual frames, and
   noticed something recurring every 103 frames, starting at
   frame 1, and something else recurring every 101 frames
   starting at 48. I just made a function to find out when
   those two would intersect at the same frame and that
   was the answer. But it occurs to me that 1 and 48 might
   be unique to my puzzle, so I reverted back to a previous
   solution where I just searched for a long string of X's
   in the resulting tree.
 *)

open Core
open Advent_lib

let line_regex = Re.Str.regexp
    "p=\\([0-9]*\\),\\([0-9]*\\) v=\\([-0-9]*\\),\\([-0-9]*\\)"
let chain_regex = Re.Str.regexp ".*XXXXXXXXXX"


let parse_line line =
  let _ = Re.Str.search_forward line_regex line 0 in
  ((Re.Str.matched_group 1 line |> Int.of_string,
    Re.Str.matched_group 2 line |> Int.of_string),
   (Re.Str.matched_group 3 line |> Int.of_string,
    Re.Str.matched_group 4 line |> Int.of_string))

let mod_pl x y =
  let r = x mod y in
  if r >= 0 then r else r + y

let move_robot days (x_bound, y_bound) ((x,y),(dx,dy)) =
  ((mod_pl (x + dx * days) x_bound,
    mod_pl (y + dy * days) y_bound), (dx, dy))

let in_quadrant ((x0,y0),(x1,y1)) ((x,y),_) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1

let quadrant_sum bounds robots =
  List.length (List.filter ~f:(in_quadrant bounds) robots)

let safety_score robots =
  quadrant_sum ((0,0),(49,50)) robots *
    quadrant_sum ((51, 0),(100,50)) robots *
      quadrant_sum ((0, 52),(49, 102)) robots *
        quadrant_sum ((51, 52), (100, 102)) robots

let has_long_chain (x_bound, y_bound) robots =
  let grid = Array.make_matrix ~dimx:x_bound ~dimy:y_bound ' ' in
  let set_robot grid ((x,y),_) = grid.(x).(y) <- 'X' in
  let array_to_list grid = Array.to_list grid in
  let array_to_string row = String.of_list (Array.to_list row) in
  let make_printable grid = List.map ~f:array_to_string (array_to_list grid) in
  let rec find_long lst =
    match lst with
    | [] -> false
    | x :: rest -> if Re.Str.string_match chain_regex x 0 then true
                   else find_long rest
  in
  List.iter ~f:(set_robot grid) robots;
  find_long (make_printable grid)

let rec do_find bounds moves robots =
  let moved_robots = List.map ~f:(move_robot moves bounds) robots in
  if has_long_chain bounds moved_robots then
    moves
  else
    do_find bounds (moves+1) robots

let print_grid (x_bound, y_bound) robots =
  let grid = Array.make_matrix ~dimx:x_bound ~dimy:y_bound ' ' in
  let set_robot grid ((x,y),_) = grid.(x).(y) <- 'X' in
  let array_to_list grid = Array.to_list grid in
  let array_to_string row = String.of_list (Array.to_list row) in
  let make_printable grid = List.map ~f:array_to_string (array_to_list grid) in
  let print grid =
    List.iter ~f:(Printf.printf "%s\n") grid in
  List.iter ~f:(set_robot grid) robots;
  print (make_printable grid)

let print_at bounds moves robots =
  let moved_robots = List.map ~f:(move_robot moves bounds) robots in
  print_grid bounds moved_robots

(*
let rec find_int move_103 move_101 =
  if move_103 == move_101 then
    move_103
  else if move_103 < move_101 then
    find_int (move_103 + 103) move_101
  else
    find_int move_103 (move_101 + 101)
 *)

let day14a () =
  let lines = Mwlib.read_file "data/day14.txt" in
  let robots = List.map ~f:parse_line lines in
  let move_100 = List.map ~f:(move_robot 100 (101, 103)) robots in
  let resulta = safety_score move_100 in
  Printf.printf "day14a = %d\n" resulta;;

let day14b () =
  let lines = Mwlib.read_file "data/day14.txt" in
  let robots = List.map ~f:parse_line lines in
  let resultb = do_find (101,103) 1 robots in
  print_at (101,103) resultb robots;
  Printf.printf "day14b = %d\n" resultb;;
  

day14a ();;
day14b ();;

