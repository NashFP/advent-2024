open Advent_lib
open Option

module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module PairsMap = Map.Make(IntPairs)

type dir_type = Up | Right | Down | Left
type space_type = Floor | Wall | Box | Robot
type b_space_type = Floor_B | Wall_B | Box_Left | Box_Right | Robot_B

let char_to_dir ch =
  if ch == '^' then Up
  else if ch == '>' then Right
  else if ch == 'v' then Down
  else Left

let move_in_dir (x,y) dir =
  match dir with
  | Up -> (x,y-1)
  | Right -> (x+1,y)
  | Down -> (x,y+1)
  | Left -> (x-1,y)

let char_to_space ch =
  if ch == '#' then Wall
  else if ch == 'O' then Box
  else if ch == '@' then Robot
  else Floor

let make_grid lines =
  let width = String.length lines.(0) in
  let height = Array.length lines in
  let get_val (x,y) = ((x,y),char_to_space lines.(y).[x]) in
  PairsMap.of_list (List.map get_val
                      (Mwlib.product (Mwlib.range 0 width)
                         (Mwlib.range 0 height)))

let make_grid_b lines =
  let width = String.length lines.(0) in
  let height = Array.length lines in
  let get_val coords (x,y) =
    match lines.(y).[x] with
    | '@' -> ((2*x,y),Robot_B) :: ((2*x+1,y),Floor_B) :: coords
    | 'O' -> ((2*x,y),Box_Left) :: ((2*x+1,y),Box_Right) :: coords
    | '#' -> ((2*x,y),Wall_B) :: ((2*x+1,y),Wall_B) :: coords
    | _ -> ((2*x,y),Floor_B) :: ((2*x+1,y),Floor_B) :: coords
  in
  PairsMap.of_list (List.fold_left get_val []
                      (Mwlib.product (Mwlib.range 0 width)
                       (Mwlib.range 0 height)))
                                                              
let find_robot grid =
  fst (List.hd (List.filter (fun (_,space) -> space == Robot)
                  (PairsMap.to_list grid)))

let find_robot_b grid =
  fst (List.hd (List.filter (fun (_,space) -> space == Robot_B)
                  (PairsMap.to_list grid)))

let rec find_floor grid dir coord =
  match PairsMap.find coord grid with
  | Floor -> Some coord
  | Box -> find_floor grid dir (move_in_dir coord dir)
  | _ -> None

let do_move (grid,(robot_x,robot_y)) dir =
  let next_coord = move_in_dir (robot_x,robot_y) dir in
  match PairsMap.find next_coord grid with
  | Wall -> (grid,(robot_x,robot_y))
  | Floor -> (PairsMap.add next_coord Robot
                (PairsMap.add (robot_x,robot_y) Floor grid), next_coord)
  | Box -> (match find_floor grid dir next_coord with
           | None -> (grid, (robot_x,robot_y))
           | Some (floor_x,floor_y) ->
              (PairsMap.add (floor_x,floor_y) Box
                (PairsMap.add next_coord Robot
                   (PairsMap.add (robot_x,robot_y) Floor grid)), next_coord))
  | Robot -> (grid,(robot_x,robot_y));;

(* If a box has to be moved horizontally, check the next square and
   if there is another box, move it first.

   For vertical moves, try moving the left half in the appropriate direction,
   trying to move any boxes encountered. If a box can't be removed, return
   None for the updated grid. Then for the right half, if the left move returned
   Some grid, try moving the right half in that grid. *)
                  
let rec try_move_box grid dir (box_x,box_y) =
  match dir with
  | Right -> (match PairsMap.find (box_x+2,box_y) grid with
             | Wall_B -> None
             | Robot_B -> None
             | Floor_B -> Some (PairsMap.add (box_x,box_y) Floor_B
                                  (PairsMap.add (box_x+1,box_y) Box_Left
                                     (PairsMap.add (box_x+2,box_y) Box_Right grid)))
             | Box_Right -> None
             | Box_Left -> (match try_move_box grid dir (box_x+2,box_y) with
                            | None -> None
                            | Some x -> Some (PairsMap.add (box_x,box_y) Floor_B
                                                (PairsMap.add (box_x+1,box_y) Box_Left
                                                   (PairsMap.add (box_x+2,box_y) Box_Right x)))))
  | Left -> (match PairsMap.find (box_x-1,box_y) grid with
             | Wall_B -> None
             | Robot_B -> None
             | Floor_B -> Some (PairsMap.add (box_x+1,box_y) Floor_B
                                  (PairsMap.add (box_x,box_y) Box_Right
                                     (PairsMap.add (box_x-1,box_y) Box_Left grid)))
             | Box_Left -> None
             | Box_Right -> (match try_move_box grid dir (box_x-2,box_y) with
                             | None -> None
                             | Some x -> Some (PairsMap.add (box_x+1,box_y) Floor_B
                                                 (PairsMap.add (box_x,box_y) Box_Right
                                                    (PairsMap.add (box_x-1,box_y) Box_Left x)))))
  | _ ->
     let (next_x,next_y) = move_in_dir (box_x,box_y) dir in
     let grid_left = match PairsMap.find (next_x,next_y) grid with
       | Wall_B -> None
       | Robot_B -> None
       | Floor_B -> Some (PairsMap.add (box_x,box_y) Floor_B
                            (PairsMap.add (next_x,next_y) Box_Left grid))
       | Box_Left -> (match try_move_box grid dir (next_x,next_y) with
                      | None -> None
                      | Some x -> Some (PairsMap.add (box_x,box_y) Floor_B
                                          (PairsMap.add (next_x,next_y) Box_Left x)))
       | Box_Right -> (match try_move_box grid dir (next_x-1,next_y) with
                       | None-> None
                       | Some x -> Some (PairsMap.add (box_x,box_y) Floor_B
                                           (PairsMap.add (next_x,next_y) Box_Left x)))
     in match grid_left with
        | None -> None
        | Some left ->
           match PairsMap.find (next_x+1,next_y) left with
           | Wall_B -> None
           | Robot_B -> None
           | Floor_B -> Some (PairsMap.add (box_x+1,box_y) Floor_B
                                (PairsMap.add (next_x+1,next_y) Box_Right left))
           | Box_Left -> (match try_move_box left dir (next_x+1,next_y) with
                          | None -> None
                          | Some x -> Some (PairsMap.add (box_x+1,box_y) Floor_B
                                              (PairsMap.add (next_x+1,next_y) Box_Right x)))
           | Box_Right -> (match try_move_box left dir (next_x,next_y) with
                           | None -> None
                           | Some x -> Some (PairsMap.add (box_x+1,box_y) Floor_B
                                               (PairsMap.add (next_x+1,next_y) Box_Right x)))
                 
let do_move_b (grid,(robot_x,robot_y)) dir =
  let (next_x,next_y) = move_in_dir (robot_x,robot_y) dir in
  match (PairsMap.find (next_x,next_y) grid) with
  | Wall_B -> (grid,(robot_x,robot_y))
  | Robot_B -> (grid,(robot_x,robot_y))
  | Floor_B -> (PairsMap.add (next_x,next_y) Robot_B
                  (PairsMap.add (robot_x,robot_y) Floor_B
                     grid),(next_x,next_y))
  | Box_Left -> (match try_move_box grid dir (next_x, next_y) with
                 | None -> (grid,(robot_x,robot_y))
                 | Some new_grid -> (PairsMap.add (next_x,next_y) Robot_B
                                       (PairsMap.add (robot_x,robot_y) Floor_B new_grid),
                                     (next_x,next_y)))
  | Box_Right -> (match try_move_box grid dir (next_x-1, next_y) with
                  | None -> (grid,(robot_x,robot_y))
                  | Some new_grid -> (PairsMap.add (next_x,next_y) Robot_B
                                        (PairsMap.add (robot_x,robot_y) Floor_B new_grid),
                                      (next_x,next_y)))
     
  
let space_to_gps ((x,y),space) =
  match space with
  | Box -> x + 100 * y
  | _ -> 0

let spaceb_to_gps ((x,y),space) =
  match space with
  | Box_Left -> x + 100 * y
  | _ -> 0

let day15 () =
  let lines = Mwlib.read_file "data/day15.txt" in
  let groups = Mwlib.split_groups lines in
  let grid = make_grid (Array.of_list (List.hd groups)) in
  let dirs = List.map char_to_dir
               (Mwlib.string_to_list (String.concat "" (List.nth groups 1))) in
  let (final_grid,_) = List.fold_left do_move (grid, find_robot grid) dirs in
  let resulta = List.fold_left (+) 0
                  (List.map space_to_gps (PairsMap.to_list final_grid)) in
  let grid_b = make_grid_b (Array.of_list (List.hd groups)) in
  let (final_grid_b,_) = List.fold_left do_move_b (grid_b, find_robot_b grid_b) dirs in
  let resultb = List.fold_left (+) 0
                  (List.map spaceb_to_gps (PairsMap.to_list final_grid_b))
  in
  Printf.printf "day15a = %d\nday15b = %d\n" resulta resultb;;
  
day15 ();;
