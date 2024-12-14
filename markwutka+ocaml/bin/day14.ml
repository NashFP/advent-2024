(* Part A was pretty easy, just multiplying the robot's
   move delta by the number of seconds and doing a modulo
   to keep it in range (I had to make a modulo func that always
   returned a positive number).

   For the second half, I converted the robot grid to a list of
   strings and looked for a long sequence of X's. *)

open Advent_lib

let line_regex = Str.regexp "p=\\([0-9]*\\),\\([0-9]*\\) v=\\([-0-9]*\\),\\([-0-9]*\\)"
let chain_regex = Str.regexp ".*XXXXXXXXXX"

let parse_line line =
  let _ = Str.search_forward line_regex line 0 in
  ((Str.matched_group 1 line |> int_of_string,
    Str.matched_group 2 line |> int_of_string),
   (Str.matched_group 3 line |> int_of_string,
    Str.matched_group 4 line |> int_of_string))

let mod_pl x y =
  let r = x mod y in
  if r >= 0 then r else r + y

let move_robot days (x_bound, y_bound) ((x,y),(dx,dy)) =
  ((mod_pl (x + dx * days) x_bound,
    mod_pl (y + dy * days) y_bound), (dx, dy))

let in_quadrant ((x0,y0),(x1,y1)) ((x,y),_) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1

let quadrant_sum bounds robots =
  List.length (List.filter (in_quadrant bounds) robots)

let safety_score robots =
  quadrant_sum ((0,0),(49,50)) robots *
    quadrant_sum ((51, 0),(100,50)) robots *
      quadrant_sum ((0, 52),(49, 102)) robots *
        quadrant_sum ((51, 52), (100, 102)) robots

let has_long_chain (x_bound, y_bound) robots =
  let grid = Array.make_matrix x_bound y_bound ' ' in
  let set_robot grid ((x,y),_) = grid.(x).(y) <- 'X' in
  let array_to_list grid = List.of_seq (Array.to_seq grid) in
  let array_to_string row = String.of_seq (Array.to_seq row) in
  let make_printable grid = List.map array_to_string (array_to_list grid) in
  let rec find_long lst =
    match lst with
    | [] -> false
    | x :: rest -> if Str.string_match chain_regex x 0 then true
                   else find_long rest
  in
  List.iter (set_robot grid) robots;
  find_long (make_printable grid)

let print_grid (x_bound, y_bound) robots =
  let grid = Array.make_matrix x_bound y_bound ' ' in
  let set_robot grid ((x,y),_) = grid.(x).(y) <- 'X' in
  let array_to_list grid = List.of_seq (Array.to_seq grid) in
  let array_to_string row = String.of_seq (Array.to_seq row) in
  let make_printable grid = List.map array_to_string (array_to_list grid) in
  let print grid =
    List.iter (Printf.printf "%s\n") grid in
  List.iter (set_robot grid) robots;
  print (make_printable grid)

let rec do_find bounds moves robots =
  let moved_robots = List.map (move_robot moves bounds) robots in
  if has_long_chain bounds moved_robots then
    moves
  else
    do_find bounds (moves+1) robots

let print_at bounds moves robots =
  let moved_robots = List.map (move_robot moves bounds) robots in
  print_grid bounds moved_robots

let day14a () =
  let lines = Mwlib.read_file "data/day14.txt" in
  let robots = List.map parse_line lines in
  let move_100 = List.map (move_robot 100 (101, 103)) robots in
  let resulta = safety_score move_100 in
  Printf.printf "day14a = %d\n" resulta;;

let day14b () =
  let lines = Mwlib.read_file "data/day14.txt" in
  let robots = List.map parse_line lines in
  let resultb = do_find (101,103) 1 robots in
  print_at (101,103) resultb robots;
  Printf.printf "day14b = %d\n" resultb;;
  

day14a ();;
day14b ();;

