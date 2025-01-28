(* In the initial solution to part A, I just generated each successive
   list of numbers, which wasn't bad. But, when part B asked for the
   process to be run 75 times, it blew up quickly.

   I then rewrote the blink process so that instead of just a list of
   numbers, it was a map of number to count. Doing that allowed it to
   still do the loop 75 times in less than a tenth of a second.
 *)

open Core
open Advent_lib

module IntMap = Map.Make(Int)

let num_digits n =
  Float.to_int (Float.round_up (Float.log10 (float_of_int (n+1))))

let split_num n num_digits =
  let pow = int_of_float (10.0 ** (float_of_int (num_digits / 2))) in
  (n / pow, n mod pow)

let update_count_multi count = function
  | None -> count
  | Some n -> n+count

let blink_step num_map (n,count) =
  if n = 0 then
    Map.update num_map 1 ~f:(update_count_multi count)
  else
    let digits = num_digits n in
    if digits mod 2 = 0 then
      let (num1, num2) = split_num n digits in
      Map.update
        (Map.update num_map num2 ~f:(update_count_multi count))
        num1 ~f:(update_count_multi count)
        
    else
      Map.update num_map (n * 2024) ~f:(update_count_multi count)

let blink num_map = List.fold ~f:blink_step ~init:IntMap.empty
    (Map.to_alist num_map)

let blink_n num_map n =
  List.fold ~f:(fun acc _ -> blink acc) ~init:num_map (List.range 0 n)
  
let make_map nums =
  IntMap.of_alist_exn (List.map ~f:(fun x -> (x,1)) nums)

let map_count num_map =
  List.reduce_exn ~f:(+) (List.map ~f:snd (Map.to_alist num_map))

let day11 () =
  let num_map = Mwlib.read_file_as_string "data/day11.txt"
             |> String.strip |> String.split ~on:' ' |>
               List.map ~f:Int.of_string |> make_map in  
  let resulta = map_count (blink_n num_map 25) in
  let resultb = map_count (blink_n num_map 75) in
  Printf.printf "day11a = %d\nday11b = %d\n" resulta resultb;;

day11 ();;
