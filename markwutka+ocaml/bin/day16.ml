(* This feels just ugly, and took me a long time.
   Basically, it's Dijkstra's algorithm, using a Map as a priority queue
   where the map is keyed/order by path_cost,x,y,direction and the
   map values are pairs of the squares on any path to this point with
   the same cost, and the visited squares for this path.
   It takes probably 5-10 minutes to run. *)

open Advent_lib

type dir_type = North | East | South | West

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

module IntDirTriple =
  struct
    type t = int * int * dir_type
    let compare (x0,y0,d0) (x1,y1,d1) =
      match Stdlib.compare x0 x1 with
      | 0 -> (match Stdlib.compare y0 y1 with
              | 0 -> compare_dir d0 d1
              | c -> c)
      | c -> c
  end
      
module IntDirSet = Set.Make(IntDirTriple)
module IntDirMap = Map.Make(IntDirTriple)
    
module IntPairs =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
  end

module PairsSet = Set.Make(IntPairs)
module PairsMap = Map.Make(IntPairs)

module WeightedIntPairs =
  struct
    type t = int * int * int * dir_type
    let compare (x0,y0,w0,d0) (x1,y1,w1,d1) =
      match Stdlib.compare w0 w1 with
      | 0 -> (match Stdlib.compare x0 x1 with
          | 0 -> (match Stdlib.compare y0 y1 with
              | 0 -> compare_dir d0 d1
              | c -> c)
          | c -> c)
      | c -> c
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
  List.hd (List.filter (fun (x,y) -> grid.(y).[x] == 'S')
             (Mwlib.product (Mwlib.range 0 width) (Mwlib.range 0 height)))

let rec find_path width height grid prio =
  let ((x,y,path_cost,dir),(path,visited)) = PriorityMap.min_binding prio in
  let new_prio = PriorityMap.remove (x,y,path_cost,dir) prio in
  let new_visited = IntDirSet.add (x,y,dir) visited in
  let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height &&
                         grid.(y).[x] != '#' in
  let try_dir prio (next_dir,next_cost) =
    let (new_x,new_y) = in_dir (x,y) next_dir in    
    if is_valid (new_x,new_y) && not (IntDirSet.mem (new_x,new_y,next_dir) new_visited) then
      match PriorityMap.find_opt (new_x,new_y,next_cost,next_dir) prio with
      | None -> PriorityMap.add (new_x,new_y,next_cost,next_dir)
                                 (PairsSet.add (new_x,new_y) path, new_visited) prio
      | Some (old_path,_) ->
        PriorityMap.add (new_x,new_y,next_cost,next_dir)
          (PairsSet.add (new_x,new_y)
             (PairsSet.add_seq (PairsSet.to_seq path) old_path), new_visited) prio
    else
      prio
  in
  if grid.(y).[x] == 'E' then
    (path_cost, 1 + List.length (PairsSet.to_list path))
  else
    find_path width height grid
      (List.fold_left try_dir new_prio [(dir,path_cost+1);(turn_left dir, path_cost+1001);
                                        (turn_right dir, path_cost+1001)])

let day16 () =
  let grid = Array.of_list (Mwlib.read_file "data/day16.txt") in
  let (start_x,start_y) = find_start grid in
  let width = String.length grid.(0) in
  let height = Array.length grid in
  let (resulta,resultb) = find_path width height grid
      (PriorityMap.add (start_x,start_y,0,East)
         (PairsSet.empty, IntDirSet.empty) PriorityMap.empty) in
  Printf.printf "day16a = %d\nday16b = %d\n" resulta resultb;;

day16 ();;
