(*
  Part A was pretty straightforward implementing the machine.

  My Part B solution is probably specific to my problem, and relies
  on the fact that the program it is executing only depends on the
  value of A in each round, and specifically the lower 3 bits of A.
  So, I try each possible A value (0-8) at first, to see which ones
  generate the correct last digit, then for each one that does,
  I shift each value left by 3 bits and try all possible combinations
  of its lower 3 bits again. I repeat that process until I have generated
  a list of the correct length, and then I return all the starting A
  values that generated that one, and I just check the first one.
 *)

open Core
open Advent_lib

exception Err of string

type combo_type = Literal of int | RegA | RegB | RegC | Reserved
type instr_type = ADV of combo_type | BXL of int | BST of combo_type | JNZ of int |
                  BXC of combo_type | OUT of combo_type | BDV of combo_type |
                  CDV of combo_type

type machine_type = Machine of int * int * int * int * instr_type array * int list

let split_regex = Str.regexp ": *"

let combo_of_int i =
  match (i land 7) with
  | 0 -> Literal 0
  | 1 -> Literal 1
  | 2 -> Literal 2
  | 3 -> Literal 3
  | 4 -> RegA
  | 5 -> RegB
  | 6 -> RegC
  | 7 -> Reserved
  | _ -> raise (Err "Invalid combo value")

let make_inst op arg =
  match (op land 7) with
  | 0 -> ADV (combo_of_int arg)
  | 1 -> BXL arg
  | 2 -> BST (combo_of_int arg)
  | 3 -> JNZ arg
  | 4 -> BXC (combo_of_int arg)
  | 5 -> OUT (combo_of_int arg)
  | 6 -> BDV (combo_of_int arg)
  | 7 -> CDV (combo_of_int arg)
  | _ -> raise (Err "Invalid instruction")

let parse_instrs instr_str =
  let instr_ints = List.map ~f:Int.of_string (String.split ~on:',' instr_str) in
  let rec loop instr_ints instrs =
    match instr_ints with
    | [] -> Array.of_list (List.rev instrs)
    | [_] -> raise (Err "Dangling instruction")
    | inst :: opcode :: rest -> loop rest (make_inst inst opcode :: instrs)
  in
  loop instr_ints []

let parse_program_target groups =
  let parse_program line =
    let parsed = List.nth_exn (Str.split split_regex line) 1 in
    List.map ~f:Int.of_string (String.split ~on:',' parsed)
  in
  parse_program (List.hd_exn (List.nth_exn groups 1))
    
let parse_machine groups =
  let parse_num line =
    let parsed = Str.split split_regex line in
    int_of_string (List.nth_exn parsed 1)
  in
  let parse_program line =
    let parsed = Str.split split_regex line in
    parse_instrs (List.nth_exn parsed 1)
  in
  let reg_group = List.hd_exn groups in
  let prog_group = List.nth_exn groups 1 in
  Machine (
    (parse_num (List.hd_exn reg_group)),
    (parse_num (List.nth_exn reg_group 1)),
    (parse_num (List.nth_exn reg_group 2)),
    0,
    (parse_program (List.hd_exn prog_group)),
    [])

let rec execute (Machine (rega,regb,regc,pc,instrs,output)) =
  let evaluate_combo = function
    | Literal n -> n
    | RegA -> rega
    | RegB -> regb
    | RegC -> regc
    | Reserved -> raise (Err "Tried to use reserved value") in
  if pc / 2 >= Array.length instrs then
    List.rev output
  else
    match instrs.(pc / 2) with
    | ADV combo -> execute (
                       Machine (rega lsr (evaluate_combo combo),
                            regb, regc, pc+2, instrs, output))
    | BXL lit -> execute (
                     Machine (rega, regb lxor lit, regc, pc+2, instrs, output))
    | BST combo -> execute (
                       Machine (rega, (evaluate_combo combo) land 7, regc,
                                pc+2, instrs, output))
    | JNZ lit -> if rega = 0 then
                   execute (Machine (rega, regb, regc, pc+2, instrs, output))
                 else
                   execute (Machine (rega, regb, regc, lit, instrs, output))
    | BXC _ -> execute (Machine (rega, regb lxor regc, regc, pc+2, instrs, output))
    | OUT combo -> execute (Machine (rega, regb, regc, pc+2, instrs,
                                     ((evaluate_combo combo) land 7) :: output))
    | BDV combo -> execute (Machine (rega, rega lsr (evaluate_combo combo),
                                     regc, pc+2, instrs, output))
    | CDV combo -> execute (Machine (rega, regb, rega lsr (evaluate_combo combo),
                                     pc+2, instrs, output))

let set_rega (Machine (_,regb,regc,pc,instrs,output)) rega =
  Machine (rega,regb,regc,pc,instrs,output)

let rega_add = Mwlib.range 0 8

let rec rev_eng_rega (Machine (_,_,_,_,instrs,_)) a_values target round num_rounds =
  let target_values = List.rev (List.take (List.rev target) round) in
  let is_possible a_add =
    let exec_output = execute (Machine (a_add, 0, 0, 0, instrs, [])) in
    List.equal (fun a b -> a = b) target_values exec_output
  in
  let possible_a rega = List.map ~f:(fun x -> (rega lsl 3) + x) rega_add in
  let all_a = List.filter ~f:is_possible (List.concat_map ~f:possible_a a_values) in
  if round = num_rounds then
    all_a
  else
    rev_eng_rega (Machine (0,0,0,0,instrs,[])) all_a target (round+1) num_rounds
    
  
      
let day17 () =
  let lines = Mwlib.read_file "data/day17.txt" in
  let groups = Mwlib.split_groups lines in
  let machine = parse_machine groups in
  let outputa = execute machine in
  let resulta = String.concat ~sep:"," (List.map ~f:Int.to_string outputa) in
  let targetb = parse_program_target groups in
  let targetb_str = String.concat ~sep:"," (List.map ~f:Int.to_string targetb) in
  let resultb = (rev_eng_rega machine [0] targetb 1 (List.length targetb)) in
  let check_machineb = set_rega machine (List.hd_exn resultb) in
  let check_outputb = execute check_machineb in
  let check_resultb = String.concat ~sep:"," (List.map ~f:Int.to_string check_outputb) in
  (if String.equal check_resultb targetb_str then
    Printf.printf "Resultb check succeeded\n"
  else
    Printf.printf "Resultb value %d failed check (%s)\n" (List.hd_exn resultb) check_resultb);
  Printf.printf "day17a = %s\nday17b = %d\n" resulta (List.hd_exn resultb);;

day17 ();;
