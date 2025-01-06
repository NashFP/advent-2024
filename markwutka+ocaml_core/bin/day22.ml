(* This was much easier than yesterday. Part A was
   really just applying the round function over and
   over.

   For part B, you needed to find a sequence of four
   changes that would yield the highest total count.
   To do this, I created a map whose key is four
   numbers and whose value is the number of bananas
   in the fourth of those rounds. Then I was about
   to get about a 20% speedup by turning the keys
   into integers. Then, I switched to an array,
   which led to a more imperative style, but ran
   in about 1.5 second instead of 5.

   The only extra trick is that you can only look at
   the first occurrence of a sequence, so I had to
   build the map by checking to make sure that sequence
   wasn't already there.
 *)

open Advent_lib
open Option

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
       loop ((b,bx)::(c,cx)::(d,e)::rest) (((a+10)*8000+(b+10)*400+(c+10)*20+d+10,e) :: acc)
    | _ -> List.rev acc
  in
  loop l []

let make_map l =
  let add map (key,v) =
    if is_none map.(key) then
      (map.(key) <- Some v; map)
    else
      map
  in
  List.fold_left add (Array.make 160000 None) (make_map_keys l)

let merge_maps m1 m2 =
  let rec merge_at n =
    if is_none m1.(n) then
      m1.(n) <- m2.(n)
    else if is_some m2.(n) then
      m1.(n) <- Some ((get m1.(n)) + (get m2.(n)));
    if n == 0 then m1
    else merge_at (n-1)
  in
  merge_at 159999

  
let max_val map =
  let rec try_mapval n m =
    if n < 0 then m
    else
    match map.(n) with
    | None -> try_mapval (n-1) m
    | Some x -> if x > m then try_mapval (n-1) x
      else try_mapval (n-1) m
  in
  try_mapval 159999 0

let day22 () =
  let lines = Mwlib.read_file "data/day22.txt" in
  let nums = List.map int_of_string lines in  
  let resulta = List.fold_left (+) 0
                  (List.map (do_n_rounds 2000) nums) in
  let digits = List.map (rounds_digits 2000) nums in
  let maps = List.map make_map digits in
  let map_b = List.fold_left merge_maps (List.hd maps) (List.tl maps) in
  let resultb = max_val map_b in
  Printf.printf "day22a = %d\nday22b = %d\n" resulta resultb;;

day22 ();;
