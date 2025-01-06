open Advent_lib

let keypad = Array.of_list [(1,3);(0,2);(1,2);(2,2);(0,1);
                            (1,1);(2,1);(0,0);(1,0);(2,0);
                            (2,3)]


let up = 0
let press = 1
let left = 2
let down = 3
let right = 4

let dirpad = Array.of_list [(1,0);(2,0);(0,1);(1,1);(2,1)]

let key_of_ch ch =
  if ch >= '0' && ch <= '9' then
    Char.code ch - Char.code '0'
  else
    10

let parse_code code = List.map key_of_ch (Mwlib.string_to_list code)

let shortest_keyboard_step (from_x, from_y) (to_x, to_y) =
  let xmoves =
    if from_x == to_x then []
    else if from_x == to_x - 1 then
      [right]
    else if from_x == to_x - 2 then
      [right;right]
    else if from_x == to_x + 1 then
      [left]
    else
      [left;left] in
  let ymoves =
    if from_y == to_y then []
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
  if from_x == 1 && from_y == 3 then
    [ymoves @ xmoves @ [press]]
  else if from_x == 0 && from_y == 2 && to_y == 3 then
    [xmoves @ ymoves @ [press]]
  else if List.is_empty xmoves then
    [ymoves @ [press]]
  else if List.is_empty ymoves then
    [xmoves @ [press]]
  else
    [xmoves @ ymoves @ [press]; ymoves @ xmoves @ [press]]

let keyboard_paths code =
  let rec loop curr_code prev_pos paths =
    match curr_code with
    | [] -> paths
    | code_val :: rest ->
      let step_vals = shortest_keyboard_step prev_pos keypad.(code_val) in
      loop rest keypad.(code_val) (List.map (fun p -> p @ step_vals) paths)
  in
  loop code keypad.(10) [[]]
  

let move_keyboard moves (from_x, from_y) (to_x, to_y) =
  let rec loop curr_x curr_y curr_moves =
    if curr_x == to_x && curr_y == to_y then
      press :: curr_moves
    else
      if curr_x == 1 && curr_y == 3 && to_y < 3 then
        loop curr_x (curr_y-1) (up :: curr_moves)
      else if curr_x < to_x then
        loop (curr_x+1) curr_y (right :: curr_moves)
      else if curr_y > to_y then
        loop curr_x (curr_y-1) (up :: curr_moves)
      else if curr_y < to_y then
        loop curr_x (curr_y+1) (down :: curr_moves)
      else
        loop (curr_x-1) curr_y (left::curr_moves)
  in
  loop from_x from_y moves

let enter_code code =
  let enter_next_code (moves,prev_pos) code =
    let to_pos = keypad.(code) in
    let next_moves = move_keyboard moves prev_pos to_pos
    in (next_moves, to_pos)
  in
  List.rev (fst (List.fold_left enter_next_code ([],keypad.(10)) code))

let move_dirpad moves (from_x, from_y) (to_x, to_y) =
  let rec loop curr_x curr_y curr_moves =
    if curr_x == to_x && curr_y == to_y then
       press :: curr_moves
    else
      if curr_x == 0 then
        loop (curr_x + 1) curr_y (right :: curr_moves)
      else if curr_x < to_x then
        loop (curr_x+1) curr_y (right :: curr_moves)
      else if curr_y > to_y then
        loop curr_x (curr_y-1) (up :: curr_moves)
      else if curr_y < to_y then
        loop curr_x (curr_y+1) (down :: curr_moves)
      else
        loop (curr_x-1) curr_y (left::curr_moves)
  in
  loop from_x from_y moves

let enter_moves moves =
  let enter_next_move (moves,prev_pos) move =
    let to_pos = dirpad.(move) in
    let next_moves = move_dirpad moves prev_pos to_pos in
    (next_moves, to_pos)
  in
  List.rev (fst (List.fold_left enter_next_move ([],dirpad.(press)) moves))
        
let moves_to_string moves =
  let ch_of_move move = "^A<v>".[move] in
  String.of_seq (List.to_seq (List.map ch_of_move moves))

let int_of_code code =
  let code_num num digit =
    if digit == 10 then num
    else num * 10 + digit in
  List.fold_left code_num 0 code

let code_val code =
  let code_int = int_of_code code in
  let m1 = enter_code code in
  Printf.printf "%s\n" (moves_to_string m1);
  let m2 = enter_moves m1 in
  Printf.printf "%s\n" (moves_to_string m2);
  let moves = enter_moves m2 in
  Printf.printf "%s\n" (moves_to_string moves);
  let code_len = List.length moves in
  Printf.printf "%d times %d\n" code_int code_len;
  code_int * code_len;;

let day21 () =
  let lines = Mwlib.read_file "data/day21test.txt" in
  let codes = List.map parse_code lines in
  let resulta = List.fold_left (+) 0 (List.map code_val codes) in
  Printf.printf "day21a = %d\n" resulta;;

