open Advent_lib
open Mwlib

type dir = Up | Down

let split_line l =
  List.map int_of_string (String.split_on_char ' ' l)

let report_dir = function
  | x :: y :: _ -> if x < y then Up else Down
  | _ -> Up

let is_good report =
  let rec report_loop rep dir =
    match rep with
    | [] -> true
    | [_] -> true
    | x :: y :: rest ->
       if (dir == Up && y >= x + 1 && y <= x + 3) ||
          (dir == Down && x >= y + 1 && x <= y + 3) then
         report_loop (y :: rest) dir
       else
         false
  in
  report_loop report (report_dir report)

let is_good_with_dampener report =
  let rec report_loop rep dir found_bad =
    match rep with
    | [] -> true
    | [_] -> true
    | x :: y :: rest ->
       if (dir == Up && y >= x + 1 && y <= x + 3) ||
          (dir == Down && x >= y + 1 && x <= y + 3) then
         report_loop (y :: rest) dir found_bad
       else if not found_bad then
         report_loop (y :: rest) dir true
       else
         false
  in
  report_loop report (report_dir report) false

  
let day2a () =
  let lines = read_file "data/day2.txt" in
  let reports = List.map split_line lines in
  let good_reports = List.filter is_good reports in
  Printf.printf "%d\n" (List.length good_reports)

let day2b () =
  let lines = read_file "data/day2.txt" in
  let reports = List.map split_line lines in
  let good_reports = List.filter is_good_with_dampener reports in
  Printf.printf "%d\n" (List.length good_reports);;

day2a ();;
day2b ();;
