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
open Advent_lib

module StringKey =
  struct
    type t = string
    let compare = String.compare
  end

module StringMap = Map.Make(StringKey)

let can_form towels orig_pattern =
  let update_count num_combos opt =
    match opt with
    | None -> Some num_combos
    | Some n -> Some (num_combos + n) in
  let add_towel pattern num_combos patterns towel =
    if String.starts_with ~prefix:towel pattern then
      StringMap.update
         (String.sub pattern (String.length towel) (String.length pattern - String.length towel))
         (update_count num_combos) patterns
    else
      patterns in
  let make_next_pattern patterns (pattern,num_combos) =
    List.fold_left (add_towel pattern num_combos) patterns towels in
  let rec loop patterns =
    let num_patterns = StringMap.cardinal patterns in
    if num_patterns == 0 then
      0
    else if num_patterns == 1 && StringMap.mem "" patterns then
      StringMap.find "" patterns
    else
      let next_patterns =
        if StringMap.mem "" patterns then
          StringMap.add "" (StringMap.find "" patterns) StringMap.empty
        else
          StringMap.empty in
      loop (List.fold_left make_next_pattern next_patterns
              (StringMap.to_list patterns))
  in
  loop (StringMap.add orig_pattern 1 StringMap.empty)

let summarize (can_form, num_combos) count =
  (can_form + (if count > 0 then 1 else 0), num_combos+count)

let day19 () =
  let lines = Mwlib.read_file "data/day19.txt" in
  let groups = Mwlib.split_groups lines in
  let towels = Str.split (Str.regexp ", *") (List.hd (List.hd groups)) in
  let patterns = List.nth groups 1 in
  let (resulta,resultb) = List.fold_left summarize (0,0)
                            (List.map (can_form towels) patterns) in
  Printf.printf "day19a = %d\nday19b = %d\n" resulta resultb;;

day19 ();;


                          
