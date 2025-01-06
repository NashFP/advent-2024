(* There was no part B for day 25 *)
open Advent_lib

let parse_group (keys, locks) group =
  let arr = Array.of_list group in
  let rec depth row curr_depth =
    if arr.(curr_depth).[row] == '.' then
      curr_depth-1
    else
      depth row (curr_depth+1)
  in
  let rec height row curr_height =
    if arr.(6-curr_height).[row] == '.' then
      curr_height-1
    else
      height row (curr_height+1)
  in
  if String.equal arr.(0) "#####" then
    ((depth 0 0, depth 1 0, depth 2 0, depth 3 0, depth 4 0) :: keys, locks)
  else
    (keys, (height 0 0, height 1 0, height 2 0, height 3 0, height 4 0)::locks)

let fit ((l0,l1,l2,l3,l4),(k0,k1,k2,k3,k4)) =
  l0 + k0 <= 5 && l1 + k1 <= 5 && l2 + k2 <= 5 && l3 + k3 <= 5 && l4 + k4 <= 5
                                                                  
let day25 () =
  let lines = Mwlib.read_file "data/day25.txt" in
  let groups = Mwlib.split_groups lines in
  let (keys, locks) = List.fold_left parse_group ([],[]) groups in
  let resulta = List.length (List.filter fit (Mwlib.product keys locks)) in
  Printf.printf "day25a = %d\n" resulta;;

day25 ();;
