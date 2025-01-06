open Core
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
  let nums = List.map ~f:Int.of_string
               (String.split ~on:' ' (List.nth_exn parts 1)) in
  Eqn (int_of_string (List.hd_exn parts), nums)

let can_uncat a b =
  let bpow = Float.round_up (Float.log10 (float_of_int b)) in
  let bmod = int_of_float (10.0 ** bpow) in
  (a mod bmod) = b

let uncat a b =
  let bpow = Float.round_up (Float.log10 (float_of_int b)) in
  let bmod = int_of_float (10.0 ** bpow) in
  a / bmod
  
let can_mul a b = a mod b = 0
let can_add a b = a > b

let solve_eqn op_list (Eqn (orig_target, nums)) =
  let rec loop target nums =
    match nums with
    | [] -> 0
    | [n] -> if n = target then orig_target else 0
    | n :: rest ->
       let rec op_loop = function
         | [] -> 0
         | (op, can_func) :: op_rest ->
            (* See if this op can be applied *)
            if can_func target n then
              (* we can use this op, so apply it and see if that leads to a solution *)
              let v = loop (op target n) rest in
              if v = 0 then
                (* if this operator results in a failure, try the next operator *)
                op_loop op_rest
              else
                (* otherwise, this operator led to a successful value, return it *)
                v
            else
              (* if we can't apply this operator, just try the next one *)
              op_loop op_rest
       in
       op_loop op_list
  in
  loop orig_target (List.rev nums)
                      
    
let day7 () =
  let lines = Mwlib.read_file "data/day7.txt" in
  let eqns = List.map ~f:parse_eqn lines in
  let ops_a = [((/), can_mul); ((-), can_add)] in
  let solveda = List.map ~f:(solve_eqn ops_a) eqns in
  let resulta = List.reduce_exn ~f:(+) solveda in
  let ops_b = [((/), can_mul); (uncat, can_uncat); ((-), can_add)] in
  let solvedb = List.map ~f:(solve_eqn ops_b) eqns in
  let resultb = List.reduce_exn ~f:(+) solvedb in
  Printf.printf "day7a = %d\nday7b = %d\n" resulta resultb;;

day7 ();;
