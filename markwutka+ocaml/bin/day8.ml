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

module CharKey =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap = Map.Make(CharKey)

let add_pair (x1,y1) (x2,y2) = (x1+x2,y1+y2)
let sub_pair (x1,y1) (x2,y2) = (x1-x2,y1-y2)

let make_grid lines width height =
  let make_col y grid ch x =
    if (ch >= '0' && ch <= '9') || (ch >= 'a' && ch <= 'z') ||
         (ch >= 'A' && ch <= 'Z') then
      CharMap.add_to_list ch (x,y) grid
    else
      grid in
  let make_row grid row y =
    List.fold_left2 (make_col y) grid row
      (Mwlib.range 0 width) in
  List.fold_left2 make_row CharMap.empty lines (Mwlib.range 0 height)

let try_pair width height set (p1,p2) =
  let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height in
  let pair_diff = sub_pair p2 p1 in
  let from_p1 = sub_pair p1 pair_diff in
  let set1 = if is_valid from_p1 then PairsSet.add from_p1 set else set in
  let from_p2 = add_pair p2 pair_diff in
  if is_valid from_p2 then PairsSet.add from_p2 set1 else set1   
   
let add_pairs width height pairs =
  List.fold_left (try_pair width height) PairsSet.empty pairs

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
  let add_pair set p = PairsSet.add p set in
  List.fold_left add_pair set (p1 :: p2 :: p1_ext @ p2_ext)

let add_pairs_b width height pairs =
  List.fold_left (try_pair_b width height) PairsSet.empty pairs

let day8 () =
  let lines = List.map Mwlib.string_to_list
                (Mwlib.read_file "data/day8.txt") in
  let height = List.length lines in
  let width = List.length (List.hd lines) in
  let grid = make_grid lines width height in
  let pairs = List.concat (List.map (Mwlib.choose_pairs)
                             (List.map snd (CharMap.to_list grid))) in
  let pairs_set = add_pairs width height pairs in
  let resulta = List.length (PairsSet.elements pairs_set) in
  let pairs_setb = add_pairs_b width height pairs in
  let resultb = List.length (PairsSet.elements pairs_setb) in
  Printf.printf "day8a = %d\nday8b = %d\n" resulta resultb;;
  
day8 ();;
