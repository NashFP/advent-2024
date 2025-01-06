open Core
open Advent_lib

module PairsMap = Map.Make(Mwlib.IntPairs)

type dir_type = Up | Right | Down | Left
type space_type = Floor | Wall | Box | Robot
type b_space_type = Floor_B | Wall_B | Box_Left | Box_Right | Robot_B

let is_robot = function
  | Robot -> true
  | _ -> false

let is_robot_b = function
  | Robot_B -> true
  | _ -> false
    
let char_to_dir ch =
  if Char.(ch = '^') then Up
  else if Char.(ch = '>') then Right
  else if Char.(ch = 'v') then Down
  else Left

let move_in_dir (x,y) dir =
  match dir with
  | Up -> (x,y-1)
  | Right -> (x+1,y)
  | Down -> (x,y+1)
  | Left -> (x-1,y)

let char_to_space ch =
  if Char.(ch = '#') then Wall
  else if Char.(ch = 'O') then Box
  else if Char.(ch = '@') then Robot
  else Floor

let make_grid lines =
  let width = String.length lines.(0) in
  let height = Array.length lines in
  let get_val x y = ((x,y),char_to_space lines.(y).[x]) in
  PairsMap.of_alist_exn (List.Cartesian_product.map2
                      (List.range 0 width)
                      (List.range 0 height)
                      ~f:get_val)

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
  PairsMap.of_alist_exn (List.fold ~f:get_val ~init:[]
                      (List.cartesian_product (List.range 0 width)
                       (List.range 0 height)))
                                                              
let find_robot grid =
  fst (List.find_exn ~f:(fun (_,space) -> is_robot space)
                  (Map.to_alist grid))

let find_robot_b grid =
  fst (List.find_exn ~f:(fun (_,space) -> is_robot_b space)
                  (Map.to_alist grid))

let rec find_floor grid dir coord =
  match Map.find_exn grid coord with
  | Floor -> Some coord
  | Box -> find_floor grid dir (move_in_dir coord dir)
  | _ -> None

let do_move (grid,(robot_x,robot_y)) dir =
  let next_coord = move_in_dir (robot_x,robot_y) dir in
  match Map.find_exn grid next_coord with
  | Wall -> (grid,(robot_x,robot_y))
  | Floor -> (Map.set (Map.set grid ~key:(robot_x,robot_y) ~data:Floor)
                ~key:next_coord ~data:Robot, next_coord)
  | Box -> (match find_floor grid dir next_coord with
           | None -> (grid, (robot_x,robot_y))
           | Some (floor_x,floor_y) ->
              (Map.set ~key:(floor_x,floor_y) ~data:Box
                (Map.set ~key:next_coord ~data:Robot
                   (Map.set grid ~key:(robot_x,robot_y) ~data:Floor)), next_coord))
    
  | Robot -> (grid,(robot_x,robot_y));;

(* If a box has to be moved horizontally, check the next square and
   if there is another box, move it first.

   For vertical moves, try moving the left half in the appropriate direction,
   trying to move any boxes encountered. If a box can't be removed, return
   None for the updated grid. Then for the right half, if the left move returned
   Some grid, try moving the right half in that grid. *)
                  
let rec try_move_box grid dir (box_x,box_y) =
  match dir with
  | Right -> (match Map.find_exn grid (box_x+2,box_y) with
             | Wall_B -> None
             | Robot_B -> None
             | Floor_B -> Some (Map.set ~key:(box_x,box_y) ~data:Floor_B
                                  (Map.set ~key:(box_x+1,box_y) ~data:Box_Left
                                     (Map.set grid ~key:(box_x+2,box_y) ~data:Box_Right)))
             | Box_Right -> None
             | Box_Left -> (match try_move_box grid dir (box_x+2,box_y) with
                            | None -> None
                            | Some x -> Some (Map.set ~key:(box_x,box_y) ~data:Floor_B
                                                (Map.set ~key:(box_x+1,box_y) ~data:Box_Left
                                                   (Map.set x ~key:(box_x+2,box_y) ~data:Box_Right)))))
  | Left -> (match Map.find_exn grid (box_x-1,box_y) with
             | Wall_B -> None
             | Robot_B -> None
             | Floor_B -> Some (Map.set ~key:(box_x+1,box_y) ~data:Floor_B
                                  (Map.set ~key:(box_x,box_y) ~data:Box_Right
                                     (Map.set grid ~key:(box_x-1,box_y) ~data:Box_Left)))
             | Box_Left -> None
             | Box_Right -> (match try_move_box grid dir (box_x-2,box_y) with
                             | None -> None
                             | Some x -> Some (Map.set ~key:(box_x+1,box_y) ~data:Floor_B
                                                 (Map.set ~key:(box_x,box_y) ~data:Box_Right
                                                    (Map.set x ~key:(box_x-1,box_y) ~data:Box_Left)))))
  | _ ->
     let (next_x,next_y) = move_in_dir (box_x,box_y) dir in
     let grid_left = match Map.find_exn grid (next_x,next_y) with
       | Wall_B -> None
       | Robot_B -> None
       | Floor_B -> Some (Map.set ~key:(box_x,box_y) ~data:Floor_B
                            (Map.set grid ~key:(next_x,next_y) ~data:Box_Left))
       | Box_Left -> (match try_move_box grid dir (next_x,next_y) with
                      | None -> None
                      | Some x -> Some (Map.set ~key:(box_x,box_y) ~data:Floor_B
                                          (Map.set x ~key:(next_x,next_y) ~data:Box_Left)))
       | Box_Right -> (match try_move_box grid dir (next_x-1,next_y) with
                       | None-> None
                       | Some x -> Some (Map.set ~key:(box_x,box_y) ~data:Floor_B
                                           (Map.set x ~key:(next_x,next_y) ~data:Box_Left)))
     in match grid_left with
        | None -> None
        | Some left ->
           match Map.find_exn left (next_x+1,next_y) with
           | Wall_B -> None
           | Robot_B -> None
           | Floor_B -> Some (Map.set ~key:(box_x+1,box_y) ~data:Floor_B
                                (Map.set left ~key:(next_x+1,next_y) ~data:Box_Right))
           | Box_Left -> (match try_move_box left dir (next_x+1,next_y) with
                          | None -> None
                          | Some x -> Some (Map.set ~key:(box_x+1,box_y) ~data:Floor_B
                                              (Map.set x ~key:(next_x+1,next_y) ~data:Box_Right)))
           | Box_Right -> (match try_move_box left dir (next_x,next_y) with
                           | None -> None
                           | Some x -> Some (Map.set ~key:(box_x+1,box_y) ~data:Floor_B
                                               (Map.set x ~key:(next_x+1,next_y) ~data:Box_Right)))
                 
let do_move_b (grid,(robot_x,robot_y)) dir =
  let (next_x,next_y) = move_in_dir (robot_x,robot_y) dir in
  match Map.find_exn grid (next_x,next_y) with
  | Wall_B -> (grid,(robot_x,robot_y))
  | Robot_B -> (grid,(robot_x,robot_y))
  | Floor_B -> (Map.set ~key:(next_x,next_y) ~data:Robot_B
                  (Map.set grid ~key:(robot_x,robot_y) ~data:Floor_B)
               ,(next_x,next_y))
  | Box_Left -> (match try_move_box grid dir (next_x, next_y) with
                 | None -> (grid,(robot_x,robot_y))
                 | Some new_grid -> (Map.set ~key:(next_x,next_y) ~data:Robot_B
                                       (Map.set new_grid ~key:(robot_x,robot_y) ~data:Floor_B),
                                     (next_x,next_y)))
  | Box_Right -> (match try_move_box grid dir (next_x-1, next_y) with
                  | None -> (grid,(robot_x,robot_y))
                  | Some new_grid -> (Map.set ~key:(next_x,next_y) ~data:Robot_B
                                        (Map.set new_grid ~key:(robot_x,robot_y) ~data:Floor_B),
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
  let grid = make_grid (Array.of_list (List.hd_exn groups)) in
  let dirs = List.map ~f:char_to_dir
               (String.to_list (String.concat (List.nth_exn groups 1))) in
  let (final_grid,_) = List.fold ~f:do_move ~init:(grid, find_robot grid) dirs in
  let resulta = List.reduce_exn ~f:(+)
                  (List.map ~f:space_to_gps (Map.to_alist final_grid)) in
  let grid_b = make_grid_b (Array.of_list (List.hd_exn groups)) in
  let (final_grid_b,_) = List.fold ~f:do_move_b ~init:(grid_b, find_robot_b grid_b) dirs in
  let resultb = List.reduce_exn ~f:(+)
                  (List.map ~f:spaceb_to_gps (Map.to_alist final_grid_b))
  in
  Printf.printf "day15a = %d\nday15b = %d\n" resulta resultb;;
  
day15 ();;
