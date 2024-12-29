(*
   This one was mentally exhausting. Part A was very straightforward.
   For part B I went through all kinds of permutations of top-down or
   bottom-up. Finally, I worked out a scheme where I generate an expected
   sequence of instructions for a particular output (the input machine is
   very regular in how it computes each bit, apart from the intentionally
   swapped pairs). Then, for each output, I compare the expected sequence
   to the actual sequence and find places that can be swapped.
   *)

open Advent_lib

exception Err of string
    
module StringKey =
  struct
    type t = string
    let compare = String.compare
  end

module StringMap = Map.Make(StringKey)
module StringSet = Set.Make(StringKey)

type op_type = AND | OR | XOR
type input_source = X | Y
type gate_type = Gate of op_type * string * string | Input of input_source * int * int
type tree_type = TreeGate of op_type * tree_type * tree_type | TreeInput of input_source * int

let parse_inputs inputs =
  let parse_input_source = function
    | 'x' -> X
    | 'y' -> Y
    | _ -> raise (Err "Unexpected input source")
  in
  let split_input s =
    let parts = Str.split (Str.regexp ": *") s in
    let input_name = List.hd parts in
    let input_source = parse_input_source input_name.[0] in
    let input_num = int_of_string (String.sub input_name 1 2) in
    (input_name, Input (input_source, input_num, int_of_string (List.nth parts 1)))
  in
  StringMap.of_list (List.map split_input inputs)

let parse_circuit map lines =
  let parse_op = function
    | "XOR" -> XOR
    | "OR" -> OR
    | "AND" -> AND
    | _ -> raise (Err "Unexpected operation")
  in
  let parse_line line =
    let parts = Array.of_list (String.split_on_char ' ' line) in
    (parts.(4),Gate ((parse_op parts.(1)),parts.(0),parts.(2)))
  in
  let add_to_map map (key,gate) = StringMap.add key gate map in
  List.fold_left add_to_map map (List.map parse_line lines)

let rec eval_output circuit out =
  match StringMap.find out circuit with
  | Gate (AND,left,right) -> (eval_output circuit left) land
                             (eval_output circuit right)
  | Gate (OR,left,right) -> (eval_output circuit left) lor
                            (eval_output circuit right)
  | Gate (XOR,left,right) -> (eval_output circuit left) lxor
                             (eval_output circuit right)
  | Input (_,_,i) -> i

let eval_num circuit prefix =
  let eval_next result out =
    (result lsl 1) lor (eval_output circuit out) in
  List.fold_left eval_next 0
    (List.rev (List.filter (String.starts_with ~prefix:prefix)
                 (List.map fst (StringMap.to_list circuit))))

let output_name n =
  if n < 10 then "z0" ^ (string_of_int n)
  else "z" ^ (string_of_int n)

let is_output (name,_) = name.[0] == 'z'

(** The expect_ functions generate the expected sequence of
   instructions for a given output. *)
let rec expect_addbits n =
  TreeGate (XOR,(TreeInput (X,n)),(TreeInput (Y,n)))
and
  expect_carry_out n =
  TreeGate (AND,(TreeInput (X,n)),(TreeInput (Y,n)))
and
  expect_prev_carry_out n =
  if n == 2 then
    TreeGate (AND,expect_addbits (n-1), expect_carry_out (n-2))
  else
    TreeGate (AND,expect_addbits (n-1),expect_carry_in (n-1))
and
  expect_carry_in n =
  if n == 1 then
    TreeGate (OR,(expect_carry_out (n-1)),(expect_addbits (n-1)))
  else
    TreeGate (OR,(expect_carry_out (n-1)),(expect_prev_carry_out n))
and
  expect_output max_n n =
  if n == 0 then
    expect_addbits n
  else if n == 1 then
    TreeGate (XOR,(expect_addbits n),(expect_carry_out (n-1)))
  else if n < max_n then
    TreeGate (XOR,(expect_addbits n),(expect_carry_in n))
  else
    expect_carry_in n

(** The matches function returns true if a named circuit matches
    its expected value *)
let rec matches circuit expected name  =
  let node = StringMap.find name circuit in
  match (expected, node) with
  | (TreeGate (op,left,right), Gate (gate_op, gate_left, gate_right)) ->
    if op == gate_op then
      (* If the op codes match, see if the operands match, either in
         the same order or swapped *)
      (matches circuit left gate_left && matches circuit right gate_right) ||
      (matches circuit right gate_left && matches circuit left gate_right)
    else
      false
  | (TreeInput (xy,name), Input (input_xy, input_name, _)) ->
    xy == input_xy && name == input_name
  | _ -> false

(** find_match searches all the circuits looking for one that matches
    the expected value *)
let find_match circuit expected =
  let is_match (name,_) = matches circuit expected name in
  let matched = List.filter is_match (StringMap.to_list circuit) in
  if List.is_empty matched then
    None
  else
    Some (fst (List.hd matched))

(** repair looks for circuits that don't match their expected value,
    and when it finds one, it searches the circuits for the expected
    value and then swaps the two values and records the names of the
    two swapped items *)
let rec repair circuit corrections expected name =
  if matches circuit expected name then
    (circuit,corrections)
  else
    let node = StringMap.find name circuit in
    match (expected, node) with
    | (TreeGate (op,left,right), Gate (gate_op, gate_left, gate_right)) ->
      if op == gate_op then
        if matches circuit left gate_left then
          if matches circuit right gate_right then
            (circuit, corrections)
          else
            repair circuit corrections right gate_right
        else if matches circuit right gate_right then
          repair circuit corrections left gate_left
        else if matches circuit left gate_right then
          if matches circuit right gate_left then
            (circuit, corrections)
          else
            repair circuit corrections right gate_left
        else if matches circuit right gate_left then
            repair circuit corrections left gate_right
        else
          (match (find_match circuit expected) with
           | None -> raise (Err ("can't find match for " ^ name))
           | Some matched_name ->
             let old_top = StringMap.find name circuit in
             let old_match = StringMap.find matched_name circuit in
             (StringMap.add name old_match (StringMap.add matched_name old_top circuit),
              (name :: matched_name :: corrections)))
      else
        (match (find_match circuit expected) with
        | None -> raise (Err ("can't find match for " ^ name))
        | Some matched_name ->
          let old_top = StringMap.find name circuit in
          let old_match = StringMap.find matched_name circuit in
          (StringMap.add name old_match (StringMap.add matched_name old_top circuit),
           (name :: matched_name :: corrections)))
                    
    | _ -> (circuit, corrections)
          
let repair_n max_n (circuit,corrections) n =               
  let expected = expect_output max_n n in
  if matches circuit expected (output_name n) then
    (circuit,corrections)
  else
    repair circuit corrections expected (output_name n)

let repair max_n circuit =
  List.fold_left (repair_n max_n) (circuit,[]) (Mwlib.range 0 max_n)

let day24 () =
  let lines = Mwlib.read_file "data/day24.txt" in
  let groups = Mwlib.split_groups lines in
  let inputs = parse_inputs (List.hd groups) in
  let circuit = parse_circuit inputs (List.nth groups 1) in
  let resulta = eval_num circuit "z" in
  let max_n = (List.length (List.filter is_output (StringMap.to_list circuit))) - 1 in
  let (_, corrections) = repair max_n circuit in
  let correction_str = String.concat ","
      (StringSet.to_list (StringSet.of_list corrections)) in
  Printf.printf "day24a = %d\nday24b = %s\n" resulta correction_str;;

day24 ();;
