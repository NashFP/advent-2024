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

open Core
open Advent_lib

module PairsSet = Set.Make(Mwlib.IntPairs)

let make_grid lines =
  let height = Array.length lines in
  let width = String.length lines.(0) in
  let get_val x y = Mwlib.digit_to_int lines.(y).[x] in
  Mwlib.init_matrix width height ~f:get_val

let find_starts grid =
  let width = Array.length grid in
  let height = Array.length grid.(0) in
  let coords = List.cartesian_product (List.range 0 width) (List.range 0 height) in
  let is_start (x,y) = grid.(x).(y) = 0 in
  List.filter ~f:is_start coords

let find_peak do_reduce grid start_coord =
  let width = Array.length grid in
  let height = Array.length grid.(0) in
  let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height in
  let get_val (x,y) = grid.(x).(y) in
  let add_adjacent coords (x,y) =
    (x-1,y) :: (x+1,y) :: (x,y-1) :: (x,y+1) :: coords in
  let is_valid_next h pos = is_valid pos && h = get_val pos in
  let reduce l = if do_reduce then Set.to_list (PairsSet.of_list l) else l in
  let advance coords n =
    reduce (List.filter ~f:(is_valid_next n)
              (List.fold ~f:add_adjacent ~init:[] coords))
  in
  List.fold ~f:advance ~init:[start_coord] (List.range 1 10)
    
let day10 () =
  let lines = Mwlib.read_file "data/day10.txt" in
  let grid = make_grid (Array.of_list lines) in
  let starts = find_starts grid in
  let peaks = List.map ~f:(find_peak true grid) starts in
  let num_trails = List.map ~f:List.length peaks in
  let resulta = List.reduce_exn ~f:(+) num_trails in
  let peaksb = List.map ~f:(find_peak false grid) starts in
  let num_trailsb = List.map ~f:List.length peaksb in
  let resultb = List.reduce_exn ~f:(+) num_trailsb in
  Printf.printf "day10a = %d\nday10b = %d\n" resulta resultb;;
  
  day10 ();;
