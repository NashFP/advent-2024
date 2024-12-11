(* In the initial solution to part A, I just generated each successive
   list of numbers, which wasn't bad. But, when part B asked for the
   process to be run 75 times, it blew up quickly.

   I then rewrote the blink process so that instead of just a list of
   numbers, it was a map of number to count. Doing that allowed it to
   still do the loop 75 times in less than a tenth of a second.
 *)

open Advent_lib
open Option

module IntKey =
       struct
         type t = int
         let compare = Stdlib.compare
       end

module IntMap = Map.Make(IntKey)

let num_digits n =
  int_of_float (ceil (log10 (float_of_int (n+1))))

let split_num n num_digits =
  let pow = int_of_float (10.0 ** (float_of_int (num_digits / 2))) in
  (n / pow, n mod pow)

let update_count_multi count = function
  | None -> Some count
  | Some n -> Some (n+count)

let blink_step num_map (n,count) =
  if n == 0 then
    IntMap.update 1 (update_count_multi count) num_map
  else
    let digits = num_digits n in
    if digits mod 2 == 0 then
      let (num1, num2) = split_num n digits in
      IntMap.update num1 (update_count_multi count)
        (IntMap.update num2 (update_count_multi count) num_map)
    else
      IntMap.update (n * 2024) (update_count_multi count) num_map

let blink num_map = List.fold_left blink_step IntMap.empty (IntMap.to_list num_map)

let blink_n num_map n =
  List.fold_left (fun acc _ -> blink acc) num_map (Mwlib.range 0 n)
  
let make_map nums =
  IntMap.of_list (List.map (fun x -> (x,1)) nums)

let map_count num_map =
  List.fold_left (+) 0 (List.map snd (IntMap.to_list num_map))

let day11 () =
  let num_map = Mwlib.read_file_as_string "data/day11.txt"
             |> String.trim |> String.split_on_char ' ' |>
               List.map int_of_string |> make_map in  
  let resulta = map_count (blink_n num_map 25) in
  let resultb = map_count (blink_n num_map 75) in
  Printf.printf "day11a = %d\nday11b = %d\n" resulta resultb;;

day11 ();;
