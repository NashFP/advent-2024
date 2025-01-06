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

let get_gate circuit name =
  StringMap.find name circuit

let output_name n =
  if n < 10 then "z0" ^ (string_of_int n)
  else "z" ^ (string_of_int n)

let is_output (name,_) = name.[0] == 'z'

let not_io name =
  let ch = name.[0] in
  ch != 'x' && ch != 'y' && ch != 'z'
                            
let rec names_in circuit name =
  match StringMap.find name circuit with
  | Gate (_,left,right) ->
    StringSet.union (names_in circuit left)
      (StringSet.union (names_in circuit right) (StringSet.add name StringSet.empty))
  | _ -> StringSet.add name StringSet.empty

let fix_circuit max_n circuit x y names =
  let rec fix_loop circuit n ok_names names_left corrections =
    let try_pair (name1,name2) =
      let old_name1 = StringMap.find name1 circuit in
      let old_name2 = StringMap.find name2 circuit in
      let try_circuit = StringMap.add name1 old_name2 (StringMap.add name2 old_name1 circuit) in
      let z = eval_output try_circuit (output_name n) in
      if z == ((x + y) lsr n) land 1 then
        let curr_names = names_in try_circuit (output_name n) in
          fix_loop try_circuit (n+1) (StringSet.union ok_names curr_names)
            (StringSet.diff names_left curr_names) (name1 :: name2 :: corrections)
      else
        (false, corrections)
    in
    let rec try_pairs pairs =
      match pairs with
      | [] -> (false, corrections)
      | pair :: rest -> let (worked, working_corrections) = try_pair pair in
        if worked then
          (worked, working_corrections)
        else
          try_pairs rest in
    if n > max_n then
      (x+y == eval_num circuit "z", corrections)
    else
      let z = eval_output circuit (output_name n) in
      let curr_names = names_in circuit (output_name n) in
      let other_names = StringSet.diff names_left curr_names in
      if z == ((x + y) lsr n) land 1 then
        fix_loop circuit (n+1) (StringSet.union ok_names curr_names) other_names
          corrections
      else
        let pairs = Mwlib.product (StringSet.to_list curr_names)
            (StringSet.to_list other_names) in
        try_pairs pairs
  in
  fix_loop circuit 0 StringSet.empty names []
    
        
let day24 () =
  let lines = Mwlib.read_file "data/day24.txt" in
  let groups = Mwlib.split_groups lines in
  let inputs = parse_inputs (List.hd groups) in
  let circuit = parse_circuit inputs (List.nth groups 1) in
  let resulta = eval_num circuit "z" in
  Printf.printf "day24a = %d\n" resulta;
  let x = eval_num circuit "x" in
  let y = eval_num circuit "y" in
  let names = List.filter not_io (List.map fst (StringMap.to_list circuit)) in
  let max_n = List.length (List.filter is_output (StringMap.to_list circuit)) in
  fix_circuit max_n circuit x y (StringSet.of_list names);;

