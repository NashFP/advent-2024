open Core
open Advent_lib

let space_regex = Re.Str.regexp " +"

(** Split the line into a pair of ints *)
let split_line l =
  let parts = List.map ~f:Int.of_string (Re.Str.split space_regex l) in
  (List.hd_exn parts, List.nth_exn parts 1)

let day1a () =
  let lines = Mwlib.read_file("data/day1.txt") in
  let (col1, col2) = List.unzip (List.map ~f:split_line lines) in
  let col1_sorted = List.sort col1 ~compare:Int.compare in
  let col2_sorted = List.sort col2 ~compare:Int.compare in
  let abs_diff a b = abs (a - b) in
  let result = List.reduce_exn ~f:(+)
                 (List.map2_exn ~f:abs_diff col1_sorted col2_sorted) in
  printf "day1a = %d\n" result

(** Count the occurrences of each number in l. Since l is sorted,
    we can just keep a current n and a running sum, and add (n,sum)
    to the result each time n changes. *)                   
let count_occur l =
  let rec count_loop l n sum result =
    match l with
    | nl :: restl ->
       if nl = n then
         count_loop restl n (sum + 1) result
       else
         count_loop l nl 0 ((n,sum) :: result)
    | [] -> List.rev ((n,sum) :: result)
  in
  count_loop l (List.hd_exn l) 0 []

(** Sum the occurrences of l1 in l2. L2 is a sorted list of (n,sum)
    pairs. Since each list is sorted, we either add to the sum if
    the next n1 = n2, or advance l1 if n1 < n2 or we advance l2 if n1 > n2. *)
let sum_occur l1 l2 =
  let rec sum_loop l1 l2 sum =
    match (l1, l2) with
    | (n1 :: rest1, (n2, n2count) :: rest2) ->
       if n1 = n2 then
         sum_loop rest1 l2 (sum + n1 * n2count)
       else if n1 < n2 then
         sum_loop rest1 l2 sum
       else
         sum_loop l1 rest2 sum
    | _ -> sum
  in
  sum_loop l1 l2 0

let day1b() =
  let lines = Mwlib.read_file("data/day1.txt") in
  let (col1, col2) = List.unzip (List.map ~f:split_line lines) in
  let col1_sorted = List.sort col1 ~compare:Int.compare in
  let col2_sorted = List.sort col2 ~compare:Int.compare in
  let result = sum_occur col1_sorted (count_occur col2_sorted) in
  Printf.printf "day1b = %d\n" result;;

day1a ();;
day1b ();;
