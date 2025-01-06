open Core
open Advent_lib

let parse_order o =
  let parts = String.split ~on:'|' o in
  (Int.of_string (List.hd_exn parts), Int.of_string (List.nth_exn parts 1))

let parse_update u =
  List.map ~f:Int.of_string (String.split ~on:',' u)

(* Make a matrix of booleans indicating which pages must precede another *)
let make_precedes_table parts =
  let precedes_table = Mwlib.init_matrix 100 100 ~f:(fun _ _ -> false) in
  List.iter ~f:(fun (x,y) -> precedes_table.(y).(x) <- true) parts;
  precedes_table

let is_good_update precedes_table update =
  let get_precedes m n = precedes_table.(m).(n) in
  let rec loop update_pages previous =
    match update_pages with
    | [] -> true
(* An update is good if for each element n in the update, all the previous
   elements in the list have to preceded it *)
    | n :: rest -> if List.for_all ~f:(fun x -> x)
        (List.map ~f:(get_precedes n) previous) then
                     loop rest (n :: previous)
                   else
                     false
  in
  loop update []

let get_middle u =
  List.nth_exn u ((List.length u) / 2)

let fix_update precedes_table update =
  let get_precedes m n = precedes_table.(m).(n) in
  let has_no_precedent updates u =
    not (List.exists ~f:(fun x -> x) (List.map ~f:(get_precedes u) updates)) in
  let rec loop updates result =
    match updates with
    | [] -> List.rev result
    | rest ->
       (* Find the first element in the list that doesn't have to be
          preceded by any other element in the list *)
       let next = List.find_exn ~f:(has_no_precedent rest) rest in
       (* loop with next deleted from rest, and prepended to result *)
       loop (List.filter ~f:(fun n -> n <> next) rest) (next :: result)
  in
  loop update []

let day5 () =
  let lines = Mwlib.read_file "data/day5.txt" in
  let parts = Mwlib.split_groups lines in
  let orders = List.map ~f:parse_order (List.hd_exn parts) in
  let updates = List.map ~f:parse_update (List.nth_exn parts 1) in
  let precedes_table = make_precedes_table orders in
  let (good_updates, bad_updates) =
    List.partition_tf ~f:(is_good_update precedes_table) updates in
  let resulta = List.reduce_exn ~f:(+) (List.map ~f:get_middle good_updates) in
  let fixed_updates = List.map ~f:(fix_update precedes_table) bad_updates in
  let resultb = List.reduce_exn ~f:(+) (List.map ~f:get_middle fixed_updates) in
  Printf.printf "day5a = %d\nday5b = %d\n" resulta resultb;;

day5 ();;
  
