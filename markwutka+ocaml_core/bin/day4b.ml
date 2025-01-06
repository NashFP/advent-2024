open Advent_lib;;

(*
  My initial solution just used a couple of tail-recursive functions
  to essentially do iteration. While I think that technique is both
  speed- and space- efficient, I wanted to experiment with just
  generating the possible sets of words and filtering them.
 *)

let dirs = [(-1,-1); (0, -1); (1, -1); (-1, 0); (1, 0);
            (-1, 1); (0, 1); (1, 1) ]

let xmas = ['X'; 'M'; 'A'; 'S']

let dir_range =
  let mul_pair (x,y) n = (x*n,y*n) in
  let make_range p = List.map (mul_pair p) (Mwlib.range 0 4) in
  List.map make_range dirs

let add_pairs (x1,y1) (x2,y2) = (x1+x2, y1+y2)

let coord_range p r =
  List.map (add_pairs p) r

let valid_range width height r =
  let valid_coord (x,y) = (x >= 0) && (x < width) && (y >= 0) && (y < height) in
  Mwlib.all valid_coord r

let all_word_dirs_at p =
  List.map (coord_range p) dir_range

let all_word_dirs width height =
  let coords = Mwlib.product (Mwlib.range 0 width) (Mwlib.range 0 height) in
  List.filter (valid_range width height)
    (List.concat_map all_word_dirs_at coords)

let get_word grid r =
  let get_letter (x,y) = String.get (Array.get grid y) x in
  List.map get_letter r

let is_xmas l = List.equal (==) xmas l

let all_xmas grid width height =
  (List.map (get_word grid) (all_word_dirs width height)) |> List.filter is_xmas
 
let day4 () =
  let lines = Mwlib.read_file "data/day4.txt" in
  let grid = Array.of_list lines in
  let width = String.length (Array.get grid 0) in
  let height = Array.length grid in
  let resulta = List.length (all_xmas grid width height) in
  Printf.printf "day4a = %d\n" resulta;;

day4 ();; 
