open Core
open Advent_lib

module PairsSet = Set.Make(Mwlib.IntPairs)

module CharMap = Map.Make(Char)

let add_pair (x1,y1) (x2,y2) = (x1+x2,y1+y2)
let sub_pair (x1,y1) (x2,y2) = (x1-x2,y1-y2)

let make_grid lines width height =
  let make_col y grid ch x =
    if Char.is_alphanum ch then
      Map.add_multi grid ~key:ch ~data:(x,y)
    else
      grid in
  let make_row grid row y =
    List.fold2_exn row (List.range 0 width) ~init:grid ~f:(make_col y)
  in
  List.fold2_exn lines (List.range 0 height) ~init:CharMap.empty ~f:(make_row)

let try_pair width height set (p1,p2) =
  let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height in
  let pair_diff = sub_pair p2 p1 in
  let from_p1 = sub_pair p1 pair_diff in
  let set1 = if is_valid from_p1 then Set.add set from_p1 else set in
  let from_p2 = add_pair p2 pair_diff in
  if is_valid from_p2 then Set.add set1 from_p2 else set1   
   
let add_pairs width height pairs =
  List.fold ~f:(try_pair width height) ~init:PairsSet.empty pairs

let reduce (x,y) =
  let divider = Mwlib.gcd (abs x) (abs y) in
  (x / divider, y / divider)

let extend width height p diff =
  let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height in
  let rec loop p pairs =
    let psum = add_pair p diff in
    if is_valid psum then
      loop psum (psum :: pairs)
    else
      pairs
  in
  loop p []

let try_pair_b width height set (p1,p2) =
  let p1_ext = extend width height p1 (reduce (sub_pair p1 p2)) in
  let p2_ext = extend width height p1 (reduce (sub_pair p2 p1)) in
  let add_pair set p = Set.add set p in
  List.fold ~f:add_pair ~init:set (p1 :: p2 :: p1_ext @ p2_ext)

let add_pairs_b width height pairs =
  List.fold ~f:(try_pair_b width height) ~init:PairsSet.empty pairs

let day8 () =
  let lines = List.map ~f:String.to_list
                (Mwlib.read_file "data/day8.txt") in
  let height = List.length lines in
  let width = List.length (List.hd_exn lines) in
  let grid = make_grid lines width height in
  let pairs = List.concat (List.map ~f:(Mwlib.choose_pairs)
                             (List.map ~f:snd (Map.to_alist grid))) in
  let pairs_set = add_pairs width height pairs in
  let resulta = List.length (Set.elements pairs_set) in
  let pairs_setb = add_pairs_b width height pairs in
  let resultb = List.length (Set.elements pairs_setb) in
  Printf.printf "day8a = %d\nday8b = %d\n" resulta resultb;;
  
day8 ();;
