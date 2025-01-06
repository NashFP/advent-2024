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

let rec shortest_dirpad depth shortest path starting_pos =
  if depth == 0 then
    (shortest + List.length path, starting_pos)
  else
    let rec loop curr_path (shortest, (from_x,from_y)) =
      match curr_path with
      | [] -> (shortest, (from_x, from_y))
      | next_dir :: rest ->
        let (to_x,to_y) = dirpad.(next_dir) in
        let xpath =
          if from_x == to_x then []
          else if from_x == to_x - 1 then
            [right]
          else if from_x == to_x - 2 then
            [right;right]
          else if from_x == to_x + 1 then
            [left]
          else
            [left;left] in
        let ypath =
          if from_y == to_y then []
          else if from_y == to_y - 1 then
            [down]
          else
            [up] in
        if from_x == 0 then
          loop rest (shortest_dirpad (depth-1)
                       shortest (xpath @ ypath @ [press]) (to_x,to_y))
        else if to_x == 0 then
          loop rest (shortest_dirpad (depth-1)
                       shortest (ypath @ xpath @ [press]) (to_x,to_y))
        else
          let (xfirst_shortest, _) =
            shortest_dirpad (depth-1) shortest xpath (from_x, from_y) in
          let (xy_shortest, xypos) =
             shortest_dirpad (depth-1) xfirst_shortest (ypath@[press]) (to_x,from_y) in
          let (yfirst_shortest, _) =
            shortest_dirpad (depth-1) shortest ypath (from_x, from_y) in
          let (yx_shortest, yxpos) =
             shortest_dirpad (depth-1) yfirst_shortest (xpath@[press]) (from_x,to_y) in
          if xy_shortest <= yx_shortest then 
            loop rest (xy_shortest, xypos)
          else
            loop rest (yx_shortest, yxpos)
    in
    loop path (shortest,starting_pos)
                
            
let shortest_key_path depth (from_x,from_y) (to_x,to_y) shortest last_dirpad_pos =
  if from_x == to_x && from_y == to_y then
    (shortest, last_dirpad_pos)
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
    if from_x == 1 && from_y == 3 && to_x == 0 then
      shortest_dirpad depth shortest (ypath @ xpath @ [press]) last_dirpad_pos
    else if from_x == 0 &&  to_y == 3 then
      shortest_dirpad depth shortest (xpath @ ypath @ [press]) last_dirpad_pos
    else
      let (xfirst_shortest, xfirst_pos) =
        shortest_dirpad depth shortest (xpath @ ypath @ [press]) last_dirpad_pos in
      let (yfirst_shortest, yfirst_pos) =
        shortest_dirpad depth shortest (ypath @ xpath @ [press]) last_dirpad_pos in
      if xfirst_shortest <= yfirst_shortest then
        (xfirst_shortest, xfirst_pos)
      else
        (yfirst_shortest, yfirst_pos)                   
    
    
let keyboard_path depth code =
  let rec loop curr_code prev_pos (shortest,last_dirpad_pos) =
    match curr_code with
    | [] -> shortest
    | code_val :: rest ->
      loop rest keypad.(code_val)
        (shortest_key_path depth prev_pos keypad.(code_val) shortest last_dirpad_pos)
  in
  loop code keypad.(10) (0,dirpad.(press))
  
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
  let code_len = keyboard_path 2 code in
  code_int * code_len;;

let day21 () =
  let lines = Mwlib.read_file "data/day21test.txt" in
  let codes = List.map parse_code lines in
  let resulta = List.fold_left (+) 0 (List.map code_val codes) in
  Printf.printf "day21a = %d\n" resulta;;

