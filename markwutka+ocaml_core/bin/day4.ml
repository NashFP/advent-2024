open Core
open Advent_lib

let dirs = [(-1,-1); (0, -1); (1, -1); (-1, 0); (1, 0);
            (-1, 1); (0, 1); (1, 1) ]

let xmas = "XMAS"

let rec count_dir grid x y len (dx, dy) =
  if len = 4 then
    1
  else if (x < 0) || (x >= String.length (Array.get grid 0)) ||
            (y < 0) || (y >= Array.length grid) then
    0
  else
    let row = grid.(y) in
    let ch = row.[x] in
    if Char.equal ch xmas.[len] then
      count_dir grid (x+dx) (y+dy) (len+1) (dx, dy)
    else
      0

let rec count_xmas grid x y count =
  if y >= Array.length grid then
    count
  else if x >= String.length (Array.get grid 0) then
    count_xmas grid 0 (y + 1) count
  else
    count_xmas grid (x+1) y
      (count + (List.reduce_exn ~f:(+)
         (List.map ~f:(count_dir grid x y 0) dirs)))

let count_x_mas_pos grid x y =
  let center = String.get (Array.get grid y) x in
  let ul = String.get (Array.get grid (y-1)) (x-1) in
  let ur = String.get (Array.get grid (y-1)) (x+1) in
  let ll = String.get (Array.get grid (y+1)) (x-1) in
  let lr = String.get (Array.get grid (y+1)) (x+1) in
  if Char.equal center 'A' && ((Char.equal ul 'M' && Char.equal lr 'S') ||
        (Char.equal ul 'S' && Char.equal lr 'M')) &&
       ((Char.equal ur 'M' && Char.equal ll 'S') ||
          (Char.equal ur 'S' && Char.equal ll 'M')) then
    1
  else
    0

let rec count_x_mas grid x y count =
  if y + 1 >= Array.length grid then
    count
  else if x + 1 >= String.length (Array.get grid 0) then
    count_x_mas grid 1 (y+1) count
  else
    count_x_mas grid (x+1) y (count + (count_x_mas_pos grid x y))

let day4 () =
  let lines = Mwlib.read_file "data/day4.txt" in
  let grid = Array.of_list lines in
  let resulta = count_xmas grid 0 0 0 in
  let resultb = count_x_mas grid 1 1 0 in
  Printf.printf "day4a = %d\nday4b = %d\n" resulta resultb;;

day4 ();;
