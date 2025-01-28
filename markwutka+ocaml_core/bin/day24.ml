(*
   This one was mentally exhausting. Part A was very straightforward.
   For part B I went through all kinds of permutations of top-down or
   bottom-up. Finally, I worked out a scheme where I generate an expected
   sequence of instructions for a particular output (the input machine is
   very regular in how it computes each bit, apart from the intentionally
   swapped pairs). Then, for each output, I compare the expected sequence
   to the actual sequence and find places that can be swapped.
   *)

open Core
open Advent_lib

exception Err of string
    
module StringMap = Map.Make(String)

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
    let parts = Re.Str.split (Re.Str.regexp ": *") s in
    let input_name = List.hd_exn parts in
    let input_source = parse_input_source input_name.[0] in
    let input_num = Int.of_string (String.sub input_name ~pos:1 ~len:2) in
    (input_name, Input (input_source, input_num, Int.of_string (List.nth_exn parts 1)))
  in
  StringMap.of_alist_exn (List.map ~f:split_input inputs)

let parse_circuit map lines =
  let parse_op = function
    | "XOR" -> XOR
    | "OR" -> OR
    | "AND" -> AND
    | _ -> raise (Err "Unexpected operation")
  in
  let parse_line line =
    let parts = Array.of_list (String.split ~on:' ' line) in
    (parts.(4),Gate ((parse_op parts.(1)),parts.(0),parts.(2)))
  in
  let add_to_map map (key,gate) = Map.set ~key:key ~data:gate map in
  List.fold ~f:add_to_map ~init:map (List.map ~f:parse_line lines)

let rec eval_output circuit out =
  match Map.find_exn circuit out with
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
  List.fold ~f:eval_next ~init:0
    (List.rev (List.filter ~f:(String.is_prefix ~prefix:prefix)
                 (List.map ~f:fst (Map.to_alist circuit))))

let output_name n =
  if n < 10 then "z0" ^ (Int.to_string n)
  else "z" ^ (Int.to_string n)

let is_output (name,_) = Char.(name.[0] = 'z')

(** The expect_ functions generate the expected sequence of
   instructions for a given output. *)
let rec expect_addbits n =
  TreeGate (XOR,(TreeInput (X,n)),(TreeInput (Y,n)))
and
  expect_carry_out n =
  TreeGate (AND,(TreeInput (X,n)),(TreeInput (Y,n)))
and
  expect_prev_carry_out n =
  if n = 2 then
    TreeGate (AND,expect_addbits (n-1), expect_carry_out (n-2))
  else
    TreeGate (AND,expect_addbits (n-1),expect_carry_in (n-1))
and
  expect_carry_in n =
  if n = 1 then
    TreeGate (OR,(expect_carry_out (n-1)),(expect_addbits (n-1)))
  else
    TreeGate (OR,(expect_carry_out (n-1)),(expect_prev_carry_out n))
and
  expect_output max_n n =
  if n = 0 then
    expect_addbits n
  else if n = 1 then
    TreeGate (XOR,(expect_addbits n),(expect_carry_out (n-1)))
  else if n < max_n then
    TreeGate (XOR,(expect_addbits n),(expect_carry_in n))
  else
    expect_carry_in n

(** The matches function returns true if a named circuit matches
    its expected value *)
let rec matches circuit expected name  =
  let node = Map.find_exn circuit name in
  match (expected, node) with
  | (TreeGate (op,left,right), Gate (gate_op, gate_left, gate_right)) ->
    if phys_equal op gate_op then
      (* If the op codes match, see if the operands match, either in
         the same order or swapped *)
      (matches circuit left gate_left && matches circuit right gate_right) ||
      (matches circuit right gate_left && matches circuit left gate_right)
    else
      false
  | (TreeInput (xy,name), Input (input_xy, input_name, _)) ->
    phys_equal xy input_xy && name = input_name
  | _ -> false

(** find_match searches all the circuits looking for one that matches
    the expected value *)
let find_match circuit expected =
  let is_match (name,_) = matches circuit expected name in
  let matched = List.filter ~f:is_match (Map.to_alist circuit) in
  if List.is_empty matched then
    None
  else
    Some (fst (List.hd_exn matched))

let replace_with_match circuit corrections name expected =
  match find_match circuit expected with
  | None -> raise (Err ("can't find match for " ^ name))
  | Some matched_name ->
    let old_top = Map.find_exn circuit name in
    let old_match = Map.find_exn circuit matched_name in
    (Map.set ~key:name ~data:old_match
       (Map.set ~key:matched_name ~data:old_top circuit),
     (name :: matched_name :: corrections))

(** repair looks for circuits that don't match their expected value,
    and when it finds one, it searches the circuits for the expected
    value and then swaps the two values and records the names of the
    two swapped items *)
let rec repair circuit corrections expected name =
  let node = Map.find_exn circuit name in
  match (expected, node) with
  | (TreeGate (op,left,right), Gate (gate_op, gate_left, gate_right)) ->
     if phys_equal op gate_op then
       if matches circuit left gate_left then
         if matches circuit right gate_right then
           (* If the left and right match, nothing to change *)
           (circuit, corrections)
         else
           (* The left was good, but the right didn't match, find
              what needs to be repair in the right tree *)
           repair circuit corrections right gate_right
       else if matches circuit right gate_right then
         (* We already know that the left didn't match, so find
            what needs to be repaired there *)
         repair circuit corrections left gate_left
       else if matches circuit left gate_right then
         if matches circuit right gate_left then
           (* The left and right match if you swap the expected
              so nothing needs to be fixed here *)
           (circuit, corrections)
         else
           (* Otherwise, the expected left matched the right
              gate, but the expected right didn't match the
              left gate, so repair the left gate *)
           repair circuit corrections right gate_left
       else if matches circuit right gate_left then
         (* At this point, we know the expected left didn't
            match but the expected right did, so repair
            the right gate to match the expected left *)
         repair circuit corrections left gate_right
       else
         (* if we get here, the op code matched, but both the left and
            right circuits don't match the expected, so this circuit
            is the problem. Find the circuit that does have the
            correct opcode, left and right and swap it with this one *)
         replace_with_match circuit corrections name expected
     else
       (* At this point, not even the op code matches, so this circuit
          needs to be replaced with one with the correct opcode, left
          and right and swap it with this one *)
       replace_with_match circuit corrections name expected
  | _ -> (circuit, corrections)

let repair_n max_n (circuit,corrections) n =               
  let expected = expect_output max_n n in
  if matches circuit expected (output_name n) then
    (circuit,corrections)
  else
    repair circuit corrections expected (output_name n)

let repair max_n circuit =
  List.fold ~f:(repair_n max_n) ~init:(circuit,[]) (List.range 0 max_n)

let day24 () =
  let lines = Mwlib.read_file "data/day24.txt" in
  let groups = Mwlib.split_groups lines in
  let inputs = parse_inputs (List.hd_exn groups) in
  let circuit = parse_circuit inputs (List.nth_exn groups 1) in
  let resulta = eval_num circuit "z" in
  let max_n = (List.length (List.filter ~f:is_output (Map.to_alist circuit))) - 1 in
  let (_, corrections) = repair max_n circuit in
  let correction_str = String.concat ~sep:","
      (List.sort ~compare:String.compare corrections) in
  Printf.printf "day24a = %d\nday24b = %s\n" resulta correction_str;;

day24 ();;
