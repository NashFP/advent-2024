(* The description of part A of Day 13 tries to tempt you
   into an iterative solution, promising that each button
   takes less than 100 presses, but knowing the way these
   puzzles work, I was pretty sure part B would not be
   solveable that way, so I went ahead with a linear algebra
   solution. Here's the basic equation:

   x1 y1    a     x1*a + y1 * b
         *     =
   x2 y2    b     x2*a + y2 * b

   We know what the values on the right are supposed to be
   so we just need to find a and b. You do this by inverting
   the left matrix and multiplying by the matrix of the
   desired results.

   So, the first example has:
   Button A: X+94, Y+34
   Button B: X+22, Y+67
   Prize: X=8400, Y=5400

   We want to find a and b such that 94 * a + 22 * b = 8400
   and 34 * a + 67 * y = 5400. This means we have to set up the
   initial matrix a little differently:

   x1 x2   a    8400
         *   =
   y1 y2   b    5400
         
 *)
open Advent_lib
open Option

(* Regex to pluck the numbers out of each of the 3 lines *)
let line_regex = Str.regexp ".*: *[A-Z][+=]\\([0-9]*\\), [A-Z][+=]\\([0-9]*\\)"

let parse_numbers line =
  let _ = Str.search_forward line_regex line 0 in
  (Str.matched_group 1 line |> int_of_string,
   Str.matched_group 2 line |> int_of_string)

let parse_group group_lines =
  (parse_numbers (List.hd group_lines),
   parse_numbers (List.nth group_lines 1),
   parse_numbers (List.nth group_lines 2))

let round x = int_of_float (x +. 0.5)

let solution ((x1,y1),(x2,y2),(x3,y3)) =
  (* Find the determinant of the matrix.
     If it is 0, there is no solution. *)
  let det = float_of_int (x1*y2 - x2*y1) in
  if det == 0.0 then
    None
  else
    (* Compute the inverse of the matrix *)
    let x1i = (float_of_int y2) /. det in
    let x2i = (0.0 -. float_of_int y1) /. det in
    let y1i = (0.0 -. float_of_int x2) /. det in
    let y2i = (float_of_int x1) /. det in
    (* Multiply the matrix by the desired result to find a and b *)
    let a = round (x1i *. float_of_int x3 +. y1i *. float_of_int y3) in
    let b = round (x2i *. float_of_int x3 +. y2i *. float_of_int y3) in
    (* Make sure that this solution actually solves the problem *)
    if x1 * a + x2 * b == x3 && y1 * a + y2 * b == y3 then
      Some (a,b)
    else
      None

let cost = function
  | None -> 0
  | Some (a,b) -> 3*a + b

let make_b_group ((x1,y1),(x2,y2),(x3,y3)) =
  ((x1,y1),(x2,y2),(x3+10000000000000,y3+10000000000000))

let day13 () =
  let lines = Mwlib.read_file "data/day13.txt" in
  let groups = Mwlib.split_groups lines in
  let group_numbers = List.map parse_group groups in
  let solutions = List.map solution group_numbers in
  let resulta = List.fold_left (+) 0 (List.map cost solutions) in
  let group_numbers_b = List.map make_b_group group_numbers in
  let solutions_b = List.map solution group_numbers_b in
  let resultb = List.fold_left (+) 0 (List.map cost solutions_b) in
  Printf.printf "day13a = %d\nday13b = %d\n" resulta resultb;;
    
day13 ();;                            
