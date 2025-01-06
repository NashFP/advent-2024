open Advent_lib

exception Err of string
    
module StringKey =
  struct
    type t = string
    let compare = String.compare
  end

module StringMap = Map.Make(StringKey)

type op_type = AND | OR | XOR
type input_source = X | Y
type gate_type = Gate of op_type * string * string | Input of input_source * int * int

type circuit_type = AddBits of int | CarryOut of int | CarryIn of int | PrevCarryIn of int |
                    Output of int

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

let get_gate circuit corrections name =
  if StringMap.mem name corrections then
    StringMap.find (StringMap.find name corrections) circuit
  else
    StringMap.find name circuit

let output_name n =
  if n < 10 then "z0" ^ (string_of_int n)
  else "z" ^ (string_of_int n)

let is_output (name,_) = name.[0] == 'z'

let print_gate = function
  | Gate (XOR,n1,n2) -> Printf.printf "XOR %s %s\n" n1 n2
  | Gate (AND,n1,n2) -> Printf.printf "AND %s %s\n" n1 n2
  | Gate (OR,n1,n2) -> Printf.printf "OR %s %s\n" n1 n2
  | Input (X,n,v) -> Printf.printf "X%02d = %d\n" n v
  | Input (Y,n,v) -> Printf.printf "Y%02d = %d\n" n v

let rec highest_source circuit corrections op =
  let rec find_highest highest op =
    match op with
    | Input (_,n,_) -> max n highest
    | Gate (_,s1,s2) ->
      let g1 = get_gate circuit corrections s1 in
      let g2 = get_gate circuit corrections s2 in
      max highest (max (highest_source circuit corrections g1)
                     (highest_source circuit corrections g2))
  in
  find_highest 0 op
    
let find_op_with circuit corrections op source1 source2 =
  let rec loop ops =
    match ops with
    | [] -> None
    | (name,(Gate (circuit_op,s1,s2))) :: rest ->
      if circuit_op == op && ((s1 == source1 && s2 == source2) ||
                              (s1 == source2 && s2 == source1)) then
        if StringMap.mem name corrections then
          Some (StringMap.find name corrections)
        else
          Some name
      else
        loop rest
    | _ :: rest -> loop rest in
  loop (StringMap.to_list circuit)               
let find_corrections circuit num_outputs =
  let is_op_equal op1 (_,op2)  =
    match (op1, op2) with
    | (AddBits n0, AddBits n1) -> n0 == n1
    | (CarryOut n0, CarryOut n1) -> n0 == n1
    | (CarryIn n0, CarryIn n1) -> n0 == n1
    | (PrevCarryIn n0, PrevCarryIn n1) -> n0 == n1
    | (Output n0, Output n1) -> n0 == n1
    | _ -> false in
  let is_addbits n gate =
    match gate with
    | Gate (op,source1,source2) ->
      op == XOR && ((source1.[0] == 'x' && source2.[0] == 'y') ||
                    (source1.[0] == 'y' && source2.[0] == 'x')) &&
      (String.equal (String.sub source1 1 2) (String.sub source2 1 2)) &&
      n == (int_of_string (String.sub source1 1 2))
    | _ -> false in
  let is_carry_out n gate =
    match gate with
    | Gate (op,source1,source2) ->
        op == AND && ((source1.[0] == 'x' && source2.[0] == 'y') ||
                      (source1.[0] == 'y' && source2.[0] == 'x')) &&
        (String.equal (String.sub source1 1 2) (String.sub source2 1 2)) &&
        n == int_of_string (String.sub source1 1 2)
    | _ -> false in
  
  let build_op_map n op_map (name,gate) =
    if is_addbits n gate then
      StringMap.add name (AddBits n) op_map
    else if is_carry_out n gate then
      StringMap.add name (CarryOut n) op_map
    else
      op_map
  in
  let build_op_map op_map n = List.fold_left (build_op_map n) op_map
      (StringMap.to_list circuit) in
  let op_map = List.fold_left build_op_map StringMap.empty
      (Mwlib.range 0 num_outputs) in
  let find_op_name op_map corrections op =
    let name = fst (List.hd (List.filter (is_op_equal op) (StringMap.to_list op_map))) in
    if StringMap.mem name corrections then
      StringMap.find name corrections
    else
      name
  in
  let build_prev_carry_in op_map corrections n =
    if n == 1 then
      let addbits_in_name = find_op_name op_map corrections (AddBits n) in
      let carry_out_n2_name = find_op_name op_map corrections (CarryOut (n-1)) in
      match find_op_with circuit corrections AND addbits_in_name carry_out_n2_name with
      | Some name ->
        StringMap.add name (PrevCarryIn n) op_map
      | None ->
        
    

(*
let find_corrections circuit =
  let is_addbits n gate _ =
    match gate with
    | Gate (op,source1,source2) ->
      op == XOR && ((source1.[0] == 'x' && source2.[0] == 'y') ||
                    (source1.[0] == 'y' && source2.[0] == 'x')) &&
      String.sub source1 1 2 == String.sub source2 1 2 &&
      n == int_of_string (String.sub source1 1 2)
    | _ -> false in
  let is_carry_out n gate _ =
    match gate with
    | Gate (op,source1,source2) ->
        op == AND && ((source1.[0] == 'x' && source2.[0] == 'y') ||
                      (source1.[0] == 'y' && source2.[0] == 'x')) &&
        String.sub source1 1 2 == String.sub source2 1 2 &&
        n == int_of_string (String.sub source1 1 2)
    | _ -> false in
  let is_prev_carry_in n gate corrections =
    match gate with
    | Gate (op,source1,source2) ->
      let g1 = get_gate circuit corrections source1 in
      let g2 = get_gate circuit corrections source2 in
      op == AND && (((is_addbits (n-1) g1 corrections) &&
                     (is_carry_out (n-1) g2) corrections) ||
                    ((is_addbits (n-1) g2 corrections) &&
                     (is_carry_out (n-1) g1 corrections)))
    | _ -> false
  in
  let is_carry_in n gate corrections =
    if n == 0 then
      raise (Err "No carry in for z00")
    else
      match gate with
      | Gate (op,source1,source2) ->
        if op != OR then
          false
        else
          let g1 = get_gate circuit corrections source1 in
          let g2 = get_gate circuit corrections source2 in
          (is_carry_out (n-1) g1 corrections &&
           is_prev_carry_in (n-1) g2 corrections) ||
          (is_carry_out (n-1) g2 corrections &&
           is_prev_carry_in (n-1) g1 corrections)
      | _ -> false
  in
  let get_name_of matcher corrections =
    let apply_matcher (_,g) = matcher g corrections in
    fst (List.hd (List.filter apply_matcher (StringMap.to_list circuit)))
  in
  let output_name n =
    if n < 10 then "z0" ^ (string_of_int n)
    else "z" ^ (string_of_int n) in
  
  let ensure corrections matcher gate name =
    if matcher gate corrections then
      corrections
    else
      let actual_name = get_name_of matcher corrections in
      StringMap.add name actual_name (StringMap.add actual_name name corrections)
  in
    let ensure_output corrections n =
      
    let output_gate = get_gate circuit corrections (output_name n) in
    match output_gate with
    | Gate XOR source1 source2 ->
      let g1 = get_gate circuit corrections source1 in
      let g2 = get_gate circuit corrections source2 in
      if is_addbits n g1 corrections then
        ensure_carry_in corrections n g2 source2
      else if is_addbits n g2 corrections then
        ensure_carry_in corrections n g1 source1
      else
        raise (Err "Neither input for z output is an addbits")
    | raise (Err "Invalid output gate for " ^ (output_name n))
      

*)    
let day24 () =
  let lines = Mwlib.read_file "data/day24.txt" in
  let groups = Mwlib.split_groups lines in
  let inputs = parse_inputs (List.hd groups) in
  let circuit = parse_circuit inputs (List.nth groups 1) in
  let resulta = eval_num circuit "z" in
  Printf.printf "day24a = %d\n" resulta;
  let outputs = List.map fst (List.filter is_output (StringMap.to_list circuit)) in
  let op_map = find_corrections circuit (List.length outputs) in
  op_map;;
  
