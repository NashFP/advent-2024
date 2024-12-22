(* This was much easier than yesterday. Part A was
   really just applying the round function over and
   over.

   For part B, you needed to find a sequence of four
   changes that would yield the highest total count.
   To do this, I created a map whose key is four
   numbers and whose value is the number of bananas
   in the fourth of those rounds.

   The only extra trick is that you can only look at
   the first occurrence of a sequence, so I had to
   build the map by checking to make sure that sequence
   wasn't already there.
 *)

open Advent_lib

module IntQuads =
  struct
    type t = int * int * int * int
    let compare (a0,b0,c0,d0) (a1,b1,c1,d1) =
      match Stdlib.compare a0 a1 with
        0 -> (match Stdlib.compare b0 b1 with
              | 0 -> (match Stdlib.compare c0 c1 with
                      | 0 -> Stdlib.compare d0 d1
                      | c -> c)
              | c -> c)
      | c -> c
  end

module QuadsMap = Map.Make(IntQuads)

let do_round n =
  let n64 = (n lxor (n lsl 6)) land 16777215 in
  let n32 = (n64 lxor (n64 lsr 5)) land 16777215 in
  (n32 lxor (n32 lsl 11)) land 16777215

let rec do_n_rounds n x =
  if n == 0 then x else
    do_n_rounds (n-1) (do_round x)

let rounds_digits n x =
  let rec loop n x prev_digit digits =
    if n == 0 then
      List.rev digits
    else
      let next_x = do_round x in
      let next_digit = next_x mod 10 in
      let digit = next_digit - prev_digit in
      loop (n-1) next_x next_digit ((digit,next_digit) :: digits)
  in
  loop n x (x mod 10) []

let make_map_keys l =
  let rec loop l acc =
    match l with
    | (a,_)::(b,bx)::(c,cx)::(d,e)::rest ->
       loop ((b,bx)::(c,cx)::(d,e)::rest) (((a,b,c,d),e) :: acc)
    | _ -> List.rev acc
  in
  loop l []

let make_map l =
  let add map (key,v) =
    if not (QuadsMap.mem key map) then
      QuadsMap.add key v map
    else
      map
  in
  List.fold_left add QuadsMap.empty (make_map_keys l)

let merge_vals _ aopt bopt =
  match (aopt,bopt) with
  | (None,None) -> None
  | (Some a, None) -> Some a
  | (None,Some b) -> Some b
  | (Some a, Some b) -> Some (a+b)

let max_val a (_,b) = max a b

let day22 () =
  let lines = Mwlib.read_file "data/day22.txt" in
  let nums = List.map int_of_string lines in  
  let resulta = List.fold_left (+) 0
                  (List.map (do_n_rounds 2000) nums) in
  let digits = List.map (rounds_digits 2000) nums in
  let maps = List.map make_map digits in
  let map_b = List.fold_left (QuadsMap.merge merge_vals) QuadsMap.empty maps in
  let resultb = List.fold_left max_val 0 (QuadsMap.to_list map_b) in
  Printf.printf "day22a = %d\nday22b = %d\n" resulta resultb;;

day22 ();;
