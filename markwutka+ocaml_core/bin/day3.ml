open Core
open Advent_lib

let mul_regex = Re.Str.regexp "mul(\\([0-9]+\\),\\([0-9]+\\))"
let do_regex = Re.Str.regexp "do()"
let dont_regex = Re.Str.regexp "don't()"

let day3 () =
  let text = Mwlib.read_file_as_string "data/day3.txt" in
  let rec match_loop sum pos mul_enabled use_do =
    if mul_enabled && Re.Str.string_match mul_regex text pos then
      let left = Re.Str.matched_group 1 text in
      let right = Re.Str.matched_group 2 text in
        match_loop (sum + (int_of_string left) * (int_of_string right))
          (Re.Str.match_end ()) mul_enabled use_do
    else if use_do && Re.Str.string_match do_regex text pos then
      match_loop sum (Re.Str.match_end ()) true use_do
    else if use_do && Re.Str.string_match dont_regex text pos then
      match_loop sum (Re.Str.match_end ()) false use_do
    else if pos < String.length text then
      match_loop sum (pos + 1) mul_enabled use_do
    else
      sum
  in
  let resulta = match_loop 0 0 true false in
  let resultb = match_loop 0 0 true true in
  Printf.printf "day3a = %d\nday3b = %d\n" resulta resultb;;

day3 ();;
