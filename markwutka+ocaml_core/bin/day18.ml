(* This one is pretty straightforward. For each visited point,
   I mark every point that can be visited on the next round,
   and increment the path length. When the desired end point is
   in the set of visited points, the search is over.

   For part B, I started off successively adding bytes, to the
   list of blocked, but it turned out to be much faster to start
   at the end and remove them since the blocking byte turned out
   to be pretty far down the list, and once the path is blocked
   the search returns faster.

   It turns out, I can do part B much faster by using a binary
   search. It looks like the speedup is at least 5x.
 *)

open Core
open Advent_lib

module PairsSet = Set.Make(Mwlib.IntPairs)

let parse_pair line =
  let parts =String.split ~on:',' line |> List.map ~f:Int.of_string in
  (List.hd_exn parts, List.nth_exn parts 1)

let traverse (x,y) (max_x,max_y) blocked =
  let is_valid (x,y) = x >= 0 && x <= max_x &&
                         y >= 0 && y <= max_y &&
                           not (Set.mem blocked (x,y)) in
  let try_move visited next_dirs (x,y) =
    let add_move (x,y) dirs (dx,dy) =
      let new_move = (x+dx,y+dy) in
      if is_valid new_move && not (Set.mem visited new_move) then
        Set.add dirs new_move
      else
        dirs in
    List.fold ~f:(add_move (x,y)) ~init:next_dirs [(-1,0);(0,-1);(1,0);(0,1)] in
  let rec loop visited path_len last_visited =
    if Set.mem visited (max_x,max_y) then
      path_len
    else if Set.is_empty last_visited then
      -1
    else
      let next_dirs = List.fold ~f:(try_move visited) ~init:PairsSet.empty
                        (Set.to_list last_visited) in
      let next_visited = Set.union next_dirs visited in
      loop next_visited (path_len+1) next_dirs
  in
  loop (Set.add PairsSet.empty (x,y)) 0 (Set.add PairsSet.empty (x,y))

let rec find_first_blocker (max_x, max_y) blocked min_blocks max_blocks =
  let mid = (min_blocks + max_blocks) / 2 in
  let result_mid = traverse (0,0) (max_x,max_y)
                     (PairsSet.of_list (List.take blocked mid)) in
  if result_mid >= 0 then
    let result_mid1 = traverse (0,0) (max_x,max_y)
                        (PairsSet.of_list (List.take blocked (mid+1))) in
    if result_mid1 < 0 then
      List.hd_exn (List.drop blocked mid)
    else
      find_first_blocker (max_x, max_y) blocked mid max_blocks
  else
    find_first_blocker (max_x, max_y) blocked min_blocks mid
  
let day18 () =
  let lines = Mwlib.read_file "data/day18.txt" in
  let bytes = List.map ~f:parse_pair lines in
  let blocked = PairsSet.of_list (List.take bytes 1024) in
  let resulta = traverse (0,0) (70,70) blocked in
  let (xb,yb) = find_first_blocker (70,70) bytes 0 (List.length bytes) in
  Printf.printf "day18a = %d\nday18b = %d,%d\n" resulta xb yb;;

day18 ();;
