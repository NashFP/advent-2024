let read_file filename =
  In_channel.with_open_text filename In_channel.input_lines

let read_file_as_string filename =
  In_channel.with_open_text filename In_channel.input_all

let split_groups l =
    let f (acc,lists) l =
        if String.length l = 0 then
            ([], acc::lists)
        else
            (l::acc, lists)
    in
      List.rev (""::l) |>
      List.fold_left f ([],[])
      |> snd;;

let pair x y = (x, y)

let product l1 l2 =
  let prod1 l x = List.map (pair x) l in                             
  List.concat_map (prod1 l2) l1

let rec all f l =
  match l with
  | [] -> true
  | x :: rest -> if not (f x) then false else all f rest

let rec any f l =
  match l with
  | [] -> false
  | x :: rest -> if f x then true else any f rest

let range f t =
  let rec range1 f t acc =
    if f >= t then
      List.rev acc
    else
      range1 (f+1) t (f::acc)
  in
  range1 f t [];;

let string_to_list s = List.of_seq (String.to_seq s)

let rec gcd a b =
  let m = a mod b in
  if m == 0 then b else gcd b m

let double_cons h l2 l1 = (h :: l1) :: l2

let rec choose n l =
  match n with
  | 0 -> []
  | 1 -> List.map (fun x -> [x]) l
  | n -> if List.is_empty l then [] else
           let choose_rest = choose n (List.tl l) in
           let choose_n1 = choose (n-1) (List.tl l) in
           let h = List.hd l in
           List.fold_left (double_cons h) choose_rest choose_n1;;

let rec choose_pairs l =
  let cons_pair h acc t = (pair h t) :: acc in
  if List.is_empty l then [] else
    let choose_rest = choose_pairs (List.tl l) in
    let h = List.hd l in
    List.fold_left (cons_pair h) choose_rest (List.tl l)

let rec permute l =
  let rec loop l pre acc =
    match l with
    | [] -> acc
    | x :: rest ->
       let permute_rest = permute (List.rev_append pre rest) in
                   loop rest (x :: pre) (List.fold_left (double_cons x) acc permute_rest)
  in
  match l with
  | [] -> []
  | [x] -> [[x]]
  | _ -> loop l [] []
     
let digit_to_int ch = Char.code ch - Char.code '0'

let matrix_dim m = (Array.length m, Array.length m.(0))
let matrix_to_list m =
  let (width,height) = matrix_dim m in
  let make_pair (x,y) = ((x,y),m.(x).(y)) in
  List.map make_pair (product (range 0 width) (range 0 height))
                         
  
