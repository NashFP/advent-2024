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
  let find_highest highest op =
    match op with
    | Input (_,n,_) -> max n highest
    | Gate (_,s1,s2) ->
      let g1 = get_gate circuit corrections s1 in
      let g2 = get_gate circuit corrections s2 in
      max highest (max (highest_source circuit corrections g1)
                     (highest_source circuit corrections g2))
  in
  find_highest 0 op
    
let find_mismatches circuit corrections =
  let rec loop ops acc =
    match ops with
    | [] -> acc
    | (_,op) :: rest ->
      (match op with
       | Input _ -> acc
       | Gate (XOR,source1,source2) ->
         let g1 = get_gate circuit corrections source1 in
         let g2 = get_gate circuit corrections source2 in
         let h1 = highest_source circuit corrections g1 in
         let h2 = highest_source circuit corrections g2 in
         if abs (h1 - h2) > 1 then
           loop rest ((source1,source2,h1,h2) :: acc)
         else
           loop rest acc
       | Gate (AND,source1,source2) ->
         let g1 = get_gate circuit corrections source1 in
         let g2 = get_gate circuit corrections source2 in
         let h1 = highest_source circuit corrections g1 in
         let h2 = highest_source circuit corrections g2 in
         if abs (h1 - h2) > 1 then
           loop rest ((source1,source2,h1,h2) :: acc)
         else
           loop rest acc
       | Gate (OR,source1,source2) ->
         let g1 = get_gate circuit corrections source1 in
         let g2 = get_gate circuit corrections source2 in
         let h1 = highest_source circuit corrections g1 in
         let h2 = highest_source circuit corrections g2 in
         if h1 != h2 then
           loop rest ((source1,source2,h1,h2) :: acc)
         else
           loop rest acc)
  in
  loop (StringMap.to_list circuit) []
        
let day24 () =
  let lines = Mwlib.read_file "data/day24.txt" in
  let groups = Mwlib.split_groups lines in
  let inputs = parse_inputs (List.hd groups) in
  let circuit = parse_circuit inputs (List.nth groups 1) in
  let resulta = eval_num circuit "z" in
  Printf.printf "day24a = %d\n" resulta;
(*  let outputs = List.map fst (List.filter is_output (StringMap.to_list circuit)) in
  let op_map = find_corrections circuit (List.length outputs) in
    op_map;; *)
  let mismatches = find_mismatches circuit StringMap.empty in
  mismatches;;
  
