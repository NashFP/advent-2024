(* Day 10 involves finding a path on a grid.
   Since you start at grid points with a value of 0 and each
   move is to a grid point that is one higher, it is easy to
   just do a fold over the destination heights in order
   looking for any current coordinates that can move to
   that height.

   Part b is actually simpler because I just remove the filter
   that looks for unique coordinates (the filter is implemented
   by the reduce function in find_peak).

   The basic process is:
   Find all the starting coordinates - those where the grid value
   is 0.

   For each starting coordinate, either find all peaks (part a) or
   find all unique paths to each peak (part b). We fold over a list
   of heights from 1..9, and use the advance function to take the
   existing coordinate(s) and find all new coordinates that move to
   the target height. For part a, we reduce this to unique coordinates,
   while for part b we don't because each new coordinate actually
   represents a separate path.
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

let make_grid lines =
  let height = Array.length lines in
  let width = String.length lines.(0) in
  let get_val x y = Mwlib.digit_to_int lines.(y).[x] in
  Array.init_matrix width height get_val

let find_starts grid =
  let width = Array.length grid in
  let height = Array.length grid.(0) in
  let coords = Mwlib.product (Mwlib.range 0 width) (Mwlib.range 0 height) in
  let is_start (x,y) = grid.(x).(y) == 0 in
  List.filter is_start coords

let find_peak do_reduce grid start_coord =
  let width = Array.length grid in
  let height = Array.length grid.(0) in
  let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height in
  let get_val (x,y) = grid.(x).(y) in
  let add_adjacent coords (x,y) =
    (x-1,y) :: (x+1,y) :: (x,y-1) :: (x,y+1) :: coords in
  let is_valid_next h pos = is_valid pos && h == get_val pos in
  let reduce l = if do_reduce then PairsSet.to_list (PairsSet.of_list l) else l in
  let advance coords n =
    reduce (List.filter (is_valid_next n)
              (List.fold_left add_adjacent [] coords))
  in
  List.fold_left advance [start_coord] (Mwlib.range 1 10)
    
let day10 () =
  let lines = Mwlib.read_file "data/day10.txt" in
  let grid = make_grid (Array.of_list lines) in
  let starts = find_starts grid in
  let peaks = List.map (find_peak true grid) starts in
  let num_trails = List.map List.length peaks in
  let resulta = List.fold_left (+) 0 num_trails in
  let peaksb = List.map (find_peak false grid) starts in
  let num_trailsb = List.map List.length peaksb in
  let resultb = List.fold_left (+) 0 num_trailsb in
  Printf.printf "day10a = %d\nday10b = %d\n" resulta resultb;;
  
  day10 ();;
