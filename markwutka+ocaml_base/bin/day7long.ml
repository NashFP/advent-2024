open Advent_lib

(* This implementation tries combinations of operators but quits as soon as it
   figures out that a particular operator won't work. To do this, we reverse the
   list of numbers. Then the first number in the list is now the last number that
   was used to generate the target. If the last operator was '*' then the target
   must be evenly divisible by that number. Similarly, if the last operator was
   || then the target's digits should end with that number.

   So, to solve, we take the list of numbers and original target, as well as a
   list of operators where each item in the operator list is actually a pair,
   where the first item in the pair is the inverse of the operator (i.e. / for *,
   - for +, uncat for cat) and the second is a function that returns true if the
   current target can have that inverse operator applied.

   Each time we try an operator, we apply its inverse to the target, and try the
   next number in the list. *)

type equation_type = Eqn of int * int list

let eqn_regex = Str.regexp ": *"

let parse_eqn l =
  let parts = Str.split eqn_regex l in
  let nums = List.map int_of_string
               (String.split_on_char ' ' (List.nth parts 1)) in
  Eqn (int_of_string (List.hd parts), nums)

let cat a b =
  let bpow = ceil (log10 (float_of_int b)) in
  let bmod = int_of_float (10.0 ** bpow) in
  a * bmod + b
  
let solve_eqn_a (Eqn (orig_target, nums)) =
  let rec loop target nums =
    match nums with
    | [] -> if target == orig_target then orig_target else 0
    | n :: rest ->
       if n > orig_target then
         0
       else
         let mul_val = loop (target * n) rest in
         if mul_val != 0 then
           mul_val
         else
           loop (target + n) rest
  in
  loop (List.hd nums) (List.tl nums)
                      
let solve_eqn_b (Eqn (orig_target, nums)) =
  let rec loop target nums =
    match nums with
    | [] -> if target == orig_target then orig_target else 0
    | n :: rest ->
       if n > orig_target then
         0
       else
         let mul_val = loop (target * n) rest in
         if mul_val != 0 then
           mul_val
         else
           let cat_val = loop (cat target n) rest in
           if cat_val != 0 then
             cat_val
           else
             loop (target + n) rest
  in
  loop (List.hd nums) (List.tl nums)
                      
    
let day7 () =
  let lines = Mwlib.read_file "data/day7.txt" in
  let eqns = List.map parse_eqn lines in
  let solveda = List.map solve_eqn_a eqns in
  let resulta = List.fold_left (+) 0 solveda in
  let solvedb = List.map solve_eqn_b eqns in
  let resultb = List.fold_left (+) 0 solvedb in
  Printf.printf "day7a = %d\nday7b = %d\n" resulta resultb;;

day7 ();;
