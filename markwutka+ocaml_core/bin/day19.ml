(* My basic strategy here is to take a pattern and the
   set of towels, see if the pattern starts with any of the
   towels and make a new set of patterns with those towels
   removed from the front.

   If the patterns are just a list, there are numerous
   duplicates. Switching to a set, I was able to eliminate the
   duplicate patterns that could be produced, but then in order
   to count the number of combinations, I turned the set
   into a map where the key is the pattern and the value is
   the number of ways I got to that pattern.
*)
open Core
open Advent_lib

module StringMap = Map.Make(String)

let can_form towels orig_pattern =
  let update_count num_combos opt =
    match opt with
    | None -> num_combos
    | Some n -> (num_combos + n) in
  let add_towel pattern num_combos patterns towel =
    if String.is_prefix pattern ~prefix:towel then
      Map.update patterns
        (String.sub pattern ~pos:(String.length towel)
                ~len:(String.length pattern - String.length towel))
         ~f:(update_count num_combos)
    else
      patterns in
  let make_next_pattern ~key ~data patterns =
    List.fold ~f:(add_towel key data) ~init:patterns towels in
  let rec loop patterns =
    let num_patterns = Map.length patterns in
    if num_patterns = 0 then
      0
    else if num_patterns = 1 && Map.mem patterns "" then
      Map.find_exn patterns ""
    else
      let next_patterns =
        if Map.mem patterns "" then
          Map.set ~key:"" ~data:(Map.find_exn patterns "") StringMap.empty
        else
          StringMap.empty in
      loop (Map.fold ~f:make_next_pattern patterns ~init:next_patterns)
  in
  loop (Map.set ~key:orig_pattern ~data:1 StringMap.empty)

let summarize (can_form, num_combos) count =
  (can_form + (if count > 0 then 1 else 0), num_combos+count)

let day19 () =
  let lines = Mwlib.read_file "data/day19.txt" in
  let groups = Mwlib.split_groups lines in
  let towels = Re.Str.split (Re.Str.regexp ", *")
      (List.hd_exn (List.hd_exn groups)) in
  let patterns = List.nth_exn groups 1 in
  let (resulta,resultb) = List.fold ~f:summarize ~init:(0,0)
                            (List.map ~f:(can_form towels) patterns) in
  Printf.printf "day19a = %d\nday19b = %d\n" resulta resultb;;

day19 ();;


                          
