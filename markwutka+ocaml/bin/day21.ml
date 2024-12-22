(*
  Day 21 always seems to give me fits. I was busy on the 21st
  so I didn't finish this until the morning of the 22nd.
  I had guessed that part B would be adding more levels of
  indirection and I guessed right.

  Although this problem uses shortest paths, you can just
  use Manhattan distance, so you never alternate
  left-down-left-down. I made two tables of the possible
  paths between any two direction pad keys, and then
  I build a table of the costs between those paths,
  using a previous table (the original is all 1s) and
  taking the min between those two paths. So for part A,
  I just create a cost table with a depth of 2, and for
  part B I create it for 25.
 *)

open Advent_lib

let keypad = Array.of_list [(1,3);(0,2);(1,2);(2,2);(0,1);
                            (1,1);(2,1);(0,0);(1,0);(2,0);
                            (2,3)]

let up = 0
let press = 1
let left = 2
let down = 3
let right = 4

let paths = [(0,0,[press]);(0,1,[right;press]);(0,2,[down;left;press]);
             (0,3,[down;press]);(0,4,[down;right;press]);(1,0,[left;press]);
             (1,1,[press]);(1,2,[down;left;left;press]);
             (1,3,[down;left;press]);(1,4,[down;press]); (2,0,[right;up;press]);
             (2,1,[right;right;up;press]);(2,2,[press]);(2,3,[right;press]);
             (2,4,[right;right;press]);(3,0,[up;press]);(3,1,[up;right;press]);
             (3,2,[left;press]);(3,3,[press]);(3,4,[right;press]);
             (4,0,[left;up;press]);(4,1,[up;press]);(4,2,[left;left;press]);
             (4,3,[left;press]);(4,4,[press])];;

let alt_paths = [(0,0,[press]);(0,1,[right;press]);(0,2,[down;left;press]);
             (0,3,[down;press]);(0,4,[right;down;press]);(1,0,[left;press]);
             (1,1,[press]);(1,2,[down;left;left;press]);
             (1,3,[left;down;press]);(1,4,[down;press]); (2,0,[right;up;press]);
             (2,1,[right;right;up;press]);(2,2,[press]);(2,3,[right;press]);
             (2,4,[right;right;press]);(3,0,[up;press]);(3,1,[right;up;press]);
             (3,2,[left;press]);(3,3,[press]);(3,4,[right;press]);
             (4,0,[up;left;press]);(4,1,[up;press]);(4,2,[left;left;press]);
             (4,3,[left;press]);(4,4,[press])];;


let path_table =
  let table = Array.make_matrix 5 5 [] in
  let set_path (x,y,path) = table.(x).(y) <- path in
  List.iter set_path paths;
  table;;

let alt_path_table =
  let table = Array.make_matrix 5 5 [] in
  let set_path (x,y,path) = table.(x).(y) <- path in
  List.iter set_path alt_paths;
  table;;


let path_cost path_table path =
  let rec loop curr_path prev_pos cost =
    match curr_path with
    | [] -> cost
    | next_pos :: rest ->
       loop rest next_pos
         (cost + path_table.(prev_pos).(next_pos))
  in
  loop path press 0

let make_cost_table n =
  let get_cost table from_pos to_pos =
    (* Compute the cost for both possible paths on the
       dirpad (some paths are the same in both tables) *)
    min (path_cost table path_table.(from_pos).(to_pos))
      (path_cost table alt_path_table.(from_pos).(to_pos)) in
  let rec loop n prev_table =
    if n == 0 then prev_table
    else
      loop (n-1)
        (Array.init_matrix 5 5 (get_cost prev_table))
  in
  loop n (Array.make_matrix 5 5 1)

let key_of_ch ch =
  if ch >= '0' && ch <= '9' then
    Char.code ch - Char.code '0'
  else
    10

let parse_code code = List.map key_of_ch (Mwlib.string_to_list code)

let shortest_key_cost table (from_x,from_y) (to_x,to_y) =
  if from_x == to_x && from_y == to_y then
    0
  else
    let xpath =
      if from_x == to_x then
        []
      else if from_x == to_x - 1 then
        [right]
      else if from_x == to_x - 2 then
        [right;right]
      else if from_x == to_x + 1 then
        [left]
      else
        [left;left]
    in
    let ypath =
      if from_y == to_y then
        []
      else if from_y == to_y - 1 then
        [down]
      else if from_y == to_y - 2 then
        [down;down]
      else if from_y == to_y - 3 then
        [down;down;down]
      else if from_y == to_y + 1 then
        [up]
      else if from_y == to_y + 2 then
        [up;up]
      else
        [up;up;up] in    
    if from_y == 3 && to_x == 0 then
      (* If we are on the bottom of the keypad and need to
         go to the far left column, we have to go up first *)
      path_cost table (ypath @ xpath @ [press])
    else if from_x == 0 &&  to_y == 3 then
      (* If we are in the far left column and have to go to
         the bottom, we have to go right first *)
      path_cost table (xpath @ ypath @ [press])
    else
      (* Choose between going x-direction first, or
         y-direction first *)
      min (path_cost table (xpath @ ypath @ [press]))
          (path_cost table (ypath @ xpath @ [press]))
    
let keyboard_path_cost table code =
  let rec loop curr_code prev_pos cost =
    match curr_code with
    | [] -> cost
    | code_val :: rest ->
      loop rest keypad.(code_val)
        (cost + (shortest_key_cost table prev_pos keypad.(code_val)))
  in
  loop code keypad.(10) 0
  
let int_of_code code =
  let code_num num digit =
    if digit == 10 then num
    else num * 10 + digit in
  List.fold_left code_num 0 code

let code_val table code =
  let code_int = int_of_code code in
  let code_cost = keyboard_path_cost table code in
  code_int * code_cost;;

let day21 () =
  let lines = Mwlib.read_file "data/day21.txt" in
  let codes = List.map parse_code lines in
  let table2 = make_cost_table 2 in
  let resulta = List.fold_left (+) 0 (List.map (code_val table2) codes) in
  let table25 = make_cost_table 25 in
  let resultb = List.fold_left (+) 0 (List.map (code_val table25) codes) in
  Printf.printf "day21a = %d\nday21b = %d\n" resulta resultb;;

day21 ();;
