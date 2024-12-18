(* This one is pretty straightforward. For each visited point,
   I mark every point that can be visited on the next round,
   and increment the path length. When the desired end point is
   in the set of visited points, the search is over.

   For part B, I started off successively adding bytes, to the
   list of blocked, but it turned out to be much faster to start
   at the end and remove them since the blocking byte turned out
   to be pretty far down the list, and once the path is blocked
   the search returns faster
 *)

open Advent_lib

module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module PairsSet = Set.Make(IntPairs)

let parse_pair line =
  let parts =String.split_on_char ',' line |> List.map int_of_string in
  (List.hd parts, List.nth parts 1)

let traverse (x,y) (max_x,max_y) blocked =
  let is_valid (x,y) = x >= 0 && x <= max_x &&
                         y >= 0 && y <= max_y &&
                           not (PairsSet.mem (x,y) blocked) in
  let try_move visited next_dirs (x,y) =
    let add_move (x,y) dirs (dx,dy) =
      let new_move = (x+dx,y+dy) in
      if is_valid new_move && not (PairsSet.mem new_move visited) then
        PairsSet.add new_move dirs
      else
        dirs in
    List.fold_left (add_move (x,y)) next_dirs [(-1,0);(0,-1);(1,0);(0,1)] in
  let rec loop visited path_len last_visited =
    if PairsSet.mem (max_x,max_y) visited then
      path_len
    else if List.is_empty (PairsSet.to_list last_visited) then
      -1
    else
      let next_dirs = List.fold_left (try_move visited) PairsSet.empty
                        (PairsSet.to_list last_visited) in
      let next_visited = PairsSet.add_seq (PairsSet.to_seq next_dirs)
                           visited in
      loop next_visited (path_len+1) next_dirs
  in
  loop (PairsSet.add (x,y) PairsSet.empty) 0 (PairsSet.add (x,y) PairsSet.empty)

let rec find_first_blocker (max_x, max_y) blocked num_blocks =
  let result = traverse (0,0) (max_x,max_y)
                 (PairsSet.of_list (Mwlib.take num_blocks blocked)) in
  if result >= 0 then
    List.hd (Mwlib.drop num_blocks blocked)
  else
    find_first_blocker (max_x, max_y) blocked (num_blocks-1)
  
  
let day18 () =
  let lines = Mwlib.read_file "data/day18.txt" in
  let bytes = List.map parse_pair lines in
  let blocked = PairsSet.of_list (Mwlib.take 1024 bytes) in
  let resulta = traverse (0,0) (70,70) blocked in
  let (xb,yb) = find_first_blocker (70,70) bytes (List.length bytes) in
  Printf.printf "day18a = %d\nday18b = %d,%d\n" resulta xb yb;;

day18 ();;
