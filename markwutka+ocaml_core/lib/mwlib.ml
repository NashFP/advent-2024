open Base

module IntPairs = struct
  module T = struct
    type t = int * int
    [@@deriving compare, sexp_of, of_sexp]
  end
  include T
  include Comparator.Make(T)
end  

let read_file filename =
  In_channel.with_open_text filename In_channel.input_lines

let read_file_as_string filename =
  In_channel.with_open_text filename In_channel.input_all

let split_groups l = 
  let f (acc,lists) l =
    if String.length l = 0 then
      ([], (List.rev acc)::lists)
    else
      (l::acc, lists)
  in
  match (List.fold l ~f:f ~init:([],[])) with
  | ([], lists) -> List.rev lists
  | (acc, lists) -> List.rev (List.rev acc :: lists)

let pair x y = (x, y)

let range f t =
  let rec range1 f t acc =
    if f >= t then
      List.rev acc
    else
      range1 (f+1) t (f::acc)
  in
  range1 f t [];;

let rec gcd a b =
  let m = a % b in
  if m = 0 then b else gcd b m

let double_cons h l2 l1 = (h :: l1) :: l2

let rec choose n l =
  match n with
  | 0 -> []
  | 1 -> List.map ~f:(fun x -> [x]) l
  | n -> (match l with
      | [] -> []
      | hd :: tl ->
        let choose_rest = choose n tl in
        let choose_n1 = choose (n-1) tl in
        List.fold ~f:(double_cons hd) ~init:choose_rest choose_n1);;


let rec choose_pairs l =
  let cons_pair h acc t = (pair h t) :: acc in
  match l with
  | [] -> []
  | hd :: tl ->
    let choose_rest = choose_pairs tl in
    List.fold ~f:(cons_pair hd) ~init:choose_rest tl

let rec permute l =
  let rec loop l pre acc =
    match l with
    | [] -> acc
    | x :: rest ->
       let permute_rest = permute (List.rev_append pre rest) in
       loop rest (x :: pre)
         (List.fold_left ~f:(double_cons x) ~init:acc permute_rest)
  in
  match l with
  | [] -> []
  | [x] -> [[x]]
  | _ -> loop l [] []
     
let digit_to_int ch = Char.to_int ch - Char.to_int '0'


let matrix_dim m = (Array.length m, Array.length m.(0))
let matrix_to_list m =
  let (width,height) = matrix_dim m in
  let make_pair x y = ((x,y),m.(x).(y)) in
  List.Cartesian_product.map2 ~f:make_pair (List.range 0 width)
    (List.range 0 height)

let init_matrix width height ~f =
  let init_row r = Array.init width ~f:(fun x -> f r x) in
  Array.init height ~f:init_row
  
let factorial n =
  let rec loop n acc =
    if n < 2 then acc
    else loop (n-1) (n * acc)
  in
  loop n 1
