open Advent_lib

let parse_order o =
  let parts = String.split_on_char '|' o in
  (int_of_string (List.hd parts), int_of_string (List.nth parts 1))

let parse_update u =
  List.map int_of_string (String.split_on_char ',' u)

(* Make a matrix of booleans indicating which pages must precede another *)
let make_precedes_table parts =
  let precedes_table = Array.init_matrix 100 100 (fun _ _ -> false) in
  let rec loop = function
    | [] -> precedes_table
    | (x,y) :: rest -> (precedes_table.(y).(x) <- true; loop rest) in
  loop parts

let is_good_update precedes_table update =
  let get_precedes m n = precedes_table.(m).(n) in
  let rec loop update_pages previous =
    match update_pages with
    | [] -> true
(* An update is good if for each element n in the update, all the previous
   elements in the list have to preceded it *)
    | n :: rest -> if Mwlib.all (fun x -> x) (List.map (get_precedes n) previous) then
                     loop rest (n :: previous)
                   else
                     false
  in
  loop update []

let get_middle u =
  List.nth u ((List.length u) / 2)

let fix_update precedes_table update =
  let get_precedes m n = precedes_table.(m).(n) in
  let has_no_precedent updates u =
    not (Mwlib.any (fun x -> x) (List.map (get_precedes u) updates)) in
  let rec loop updates result =
    match updates with
    | [] -> List.rev result
    | rest ->
       (* Find the first element in the list that doesn't have to be
          preceded by any other element in the list *)
       let next = List.hd (List.filter (has_no_precedent rest) rest) in
       (* loop with next deleted from rest, and prepended to result *)
       loop (List.filter (fun n -> n != next) rest) (next :: result)
  in
  loop update []

let day5 () =
  let lines = Mwlib.read_file "data/day5.txt" in
  let parts = Mwlib.split_groups lines in
  let orders = List.map parse_order (List.hd parts) in
  let updates = List.map parse_update (List.nth parts 1) in
  let precedes_table = make_precedes_table orders in
  let (good_updates, bad_updates) = List.partition (is_good_update precedes_table) updates in
  let resulta = List.fold_left (+) 0 (List.map get_middle good_updates) in
  let fixed_updates = List.map (fix_update precedes_table) bad_updates in
  let resultb = List.fold_left (+) 0 (List.map get_middle fixed_updates) in
  Printf.printf "day5a = %d\nday5b = %d\n" resulta resultb;;

day5 ();;
  
