open Advent_lib
open Option

type equation_type = Eqn of int * int list

let eqn_regex = Str.regexp ": *"

let parse_eqn l =
  let parts = Str.split eqn_regex l in
  let nums = List.map int_of_string
               (String.split_on_char ' ' (List.nth parts 1)) in
  Eqn (int_of_string (List.hd parts), nums)

let uncat a b =
  let astr = string_of_int a in
  let bstr = string_of_int b in
  int_of_string (String.sub astr 0 (String.length astr - String.length bstr))

let can_uncat a b =
  let astr = string_of_int a in
  let bstr = string_of_int b in
  (String.length astr > String.length bstr) && String.ends_with ~suffix:bstr astr

let can_mul a b = a mod b == 0
let can_add a b = a > b

let solve_eqn op_list (Eqn (orig_target, nums)) =
  let rec loop target nums =
    match nums with
    | [] -> 0
    | [n] -> if n == target then orig_target else 0
    | n :: rest ->
       let rec op_loop = function
         | [] -> 0
         | (op, can_func) :: op_rest ->
            if can_func target n then
              let v = loop (op target n) rest in
              if v == 0 then
                op_loop op_rest
              else
                v
            else
              op_loop op_rest
       in
       op_loop op_list
  in
  loop orig_target (List.rev nums)
                      
    
let day7a () =
  let lines = Mwlib.read_file "data/day7.txt" in
  let eqns = List.map parse_eqn lines in
  let ops_a = [((/), can_mul); ((-), can_add)] in
  let solveda = List.map (solve_eqn ops_a) eqns in
  let resulta = List.fold_left (+) 0 solveda in
  let ops_b = [((/), can_mul); (uncat, can_uncat); ((-), can_add)] in
  let solvedb = List.map (solve_eqn ops_b) eqns in
  let resultb = List.fold_left (+) 0 solvedb in
  Printf.printf "day7a = %d\nday7b = %d\n" resulta resultb;;
  
