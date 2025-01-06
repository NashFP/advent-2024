(* This feels just ugly, and took me a long time.
   Basically, it's Dijkstra's algorithm, using a Map as a priority queue
   where the map is keyed/order by path_cost,x,y,direction and the
   map values are pairs of the squares on any path to this point with
   the same cost, and the visited squares for this path.
   I had originally done the visited map wrong where it was included
   in each item in the priority queue, which meant it visited the same
   locations many times. Fixing that changed the runtime from 5-6 minutes
   to 0.04 seconds.
*)

open Core
open Advent_lib

type dir_type = North | East | South | West
                [@@deriving sexp_of, of_sexp]

let compare_dir d0 d1 =
  match (d0,d1) with
  | (North,North) -> 0
  | (North,East) -> -1
  | (North,South) -> -1
  | (North,West) -> 1
  | (East,East) -> 0
  | (East,South) -> -1
  | (East,West) -> -1
  | (East,North) -> 1
  | (South,South) -> 0
  | (South,West) -> -1
  | (South,North) -> 1
  | (South,East) -> 1
  | (West,West) -> 0
  | (West,North) -> -1
  | (West,East) -> 1
  | (West,South) -> 1

module IntDirTriple = struct
  module T = struct
    type t = int * int * dir_type
    [@@deriving sexp_of, of_sexp]
    let compare (x0,y0,d0) (x1,y1,d1) =
      match Int.compare x0 x1 with
      | 0 -> (match Int.compare y0 y1 with
              | 0 -> compare_dir d0 d1
              | c -> c)
      | c -> c
  end
  include T
  include Comparator.Make(T)
end
      
module IntDirSet = Set.Make(IntDirTriple)
module IntDirMap = Map.Make(IntDirTriple)
    
module PairsSet = Set.Make(Mwlib.IntPairs)
module PairsMap = Map.Make(Mwlib.IntPairs)

module WeightedIntPairs = struct
  module T = struct
    type t = int * int * int * dir_type
    [@@deriving sexp_of, of_sexp]
             
    let compare (x0,y0,w0,d0) (x1,y1,w1,d1) =
      match Int.compare w0 w1 with
      | 0 -> (match Int.compare x0 x1 with
          | 0 -> (match Int.compare y0 y1 with
              | 0 -> compare_dir d0 d1
              | c -> c)
          | c -> c)
      | c -> c
  end
  include T
  include Comparator.Make(T)
end

module PriorityMap = Map.Make(WeightedIntPairs)

let in_dir (x,y) dir =
  match dir with
  | North -> (x,y-1)
  | East -> (x+1,y)
  | South -> (x,y+1)
  | West -> (x-1,y)

let turn_left = function
  | North -> West
  | East -> North
  | South -> East
  | West -> South

let turn_right = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let find_start grid =
  let width = String.length grid.(0) in
  let height = Array.length grid in
  List.find_exn ~f:(fun (x,y) -> Char.(grid.(y).[x] = 'S'))
             (List.cartesian_product (List.range 0 width) (List.range 0 height))

let rec find_path width height grid prio visited =
  let ((x,y,path_cost,dir),path) = Map.min_elt_exn prio in
  let new_prio = Map.remove prio (x,y,path_cost,dir) in
  let new_visited = Set.add visited (x,y,dir) in
  let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height &&
                         Char.(grid.(y).[x] <> '#') in
  let try_dir prio (next_dir,next_cost) =
    let (new_x,new_y) = in_dir (x,y) next_dir in    
    if is_valid (new_x,new_y) && not (Set.mem new_visited (new_x,new_y,next_dir)) then
      match Map.find prio (new_x,new_y,next_cost,next_dir) with
      | None -> Map.set prio ~key:(new_x,new_y,next_cost,next_dir)
                                 ~data:(Set.add path (new_x,new_y))
      | Some old_path ->
        Map.set prio ~key:(new_x,new_y,next_cost,next_dir)
          ~data:(Set.add (Set.union path old_path) (new_x,new_y))

    else
      prio
  in
  if Char.(grid.(y).[x] = 'E') then
    (path_cost, 1 + Set.length path)
  else
    find_path width height grid
      (List.fold ~f:try_dir ~init:new_prio [(dir,path_cost+1);(turn_left dir, path_cost+1001);
                                            (turn_right dir, path_cost+1001)])
      new_visited

let day16 () =
  let grid = Array.of_list (Mwlib.read_file "data/day16.txt") in
  let (start_x,start_y) = find_start grid in
  let width = String.length grid.(0) in
  let height = Array.length grid in
  let (resulta,resultb) = find_path width height grid
      (Map.set ~key:(start_x,start_y,0,East)
         ~data:PairsSet.empty PriorityMap.empty) IntDirSet.empty in
  Printf.printf "day16a = %d\nday16b = %d\n" resulta resultb;;

day16 ();;
