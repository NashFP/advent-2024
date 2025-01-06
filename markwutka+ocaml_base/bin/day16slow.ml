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

module WeightedIntPairs =
  struct
    type t = int * int * int * (int*int) list * dir_type * IntDirSet.t
    let compare (x0,y0,w0,_,d0,_) (x1,y1,w1,_,d1,_) =
      if x0 == x1 && y0 == y1 && d0 == d1 then 0
      else
        match Stdlib.compare w0 w1 with
        | 0 -> (match Stdlib.compare x0 x1 with
                | 0 -> (match Stdlib.compare y0 y1 with
                        | 0 -> compare_dir d0 d1
                        | c -> c)
                | c -> c)
        | c -> c
  end

module PrioritySet = Set.Make(WeightedIntPairs)

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

          (*
            let rec find_path width height on_best shortest grid prio =
  if PrioritySet.is_empty prio then
    (shortest, List.length (PairsSet.to_list on_best))
  else
    let (x,y,path_cost,path,dir,visited) = PrioritySet.min_elt prio in
    let new_prio = PrioritySet.remove (x,y,path_cost,path,dir,visited) prio in
    let new_visited = IntDirSet.add (x,y,dir) visited in
    let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height &&
                           grid.(y).[x] != '#' in
    let try_dir prio (next_dir,next_cost) =
      let (new_x,new_y) = in_dir (x,y) next_dir in
      if is_valid (new_x,new_y) && not (IntDirSet.mem (new_x,new_y,next_dir) new_visited) then
        match PrioritySet.find_opt (new_x,new_y,next_cost,path,next_dir,new_visited) prio with
        | None -> PrioritySet.add
                    (new_x,new_y,next_cost,(new_x,new_y)::path,next_dir,new_visited) prio
        | Some (_,_,old_cost,old_len,old_dir,old_visited) ->
           if next_cost < old_cost then
             PrioritySet.add (new_x,new_y,next_cost,(new_x,new_y)::path,next_dir,new_visited)
               (PrioritySet.remove
                  (new_x,new_y,old_cost,old_len,old_dir,old_visited) prio)
           else
             prio
      else
        prio
    in
    if grid.(y).[x] == 'E' then
      (Printf.printf "Got to E with cost %d\n" path_cost;
      if shortest == -1 || path_cost == shortest then
        find_path width height (PairsSet.add_seq (List.to_seq path) on_best) path_cost
                                  grid new_prio
      else
        find_path width height on_best shortest grid new_prio)
    else
      find_path width height on_best shortest grid
        (List.fold_left try_dir new_prio [(dir,path_cost+1);(turn_left dir, path_cost+1001);
                                    (turn_right dir, path_cost+1001)])
           *)      
let rec find_path2 width height grid best_seats best_cost (x,y) dir cost seats visited cache =
  let found_better =
    match IntDirMap.find_opt (x,y,dir) cache with
    | None -> false
    | Some cached_cost -> cached_cost > cost
  in
  if found_better then
    (cost,seats,cache)
  else if grid.(y).[x] == 'E' then
    (cost,seats,cache)
  else
    let new_cache = IntDirMap.add (x,y,dir) cost cache in
    let new_visited = IntDirSet.add (x,y,dir) visited in
    let is_valid (x,y) = x >= 0 && x < width && y >= 0 && y < height &&
                           grid.(y).[x] != '#' in
    let merge (best_cost,best_seats,best_cache) (cost,seats) =
      if cost < best_cost || best_cost == (-1) then
        (cost, seats,best_cache)
      else if cost == best_cost then
        (cost, PairsSet.add_seq (PairsSet.to_seq seats) best_seats, best_cache)
      else
        (best_cost, best_seats, best_cache)
    in
    let try_dir (best_cost,best_seats,_) (dir,cost) =
      let (new_x,new_y) = in_dir (x,y) dir in
      if is_valid (new_x,new_y) && not (IntDirSet.mem (new_x,new_y,dir) new_visited) then
        let (cost,seats,cache) = find_path2 width height grid best_seats best_cost
                             (new_x,new_y) dir cost (PairsSet.add (new_x,new_y) seats)
                             new_visited new_cache
        in
        merge (best_cost,best_seats,cache) (cost,seats)
      else
        (best_cost,best_seats, cache)
    in
    List.fold_left try_dir (best_cost,best_seats,cache)
      [(dir, cost+1); (turn_left dir, cost+1001); (turn_right dir, cost+1001)]                         

let day16 () =
  let grid = Array.of_list (Mwlib.read_file "data/day16.txt") in
  let (start_x,start_y) = find_start grid in
  let width = String.length grid.(0) in
  let height = Array.length grid in
  (*
  let (resulta,resultb) = find_path width height PairsSet.empty (-1) grid
                            (PrioritySet.add (start_x,start_y,0,[(start_x,start_y)],East,
                               IntDirSet.empty)
                               PrioritySet.empty) in
   *)
  let (resulta, best_seats,_) = find_path2 width height grid PairsSet.empty (-1)
                                (start_x,start_y) East 0
                                (PairsSet.add (start_x,start_y) PairsSet.empty)
                                IntDirSet.empty IntDirMap.empty
  in
  let resultb = List.length (PairsSet.to_list best_seats) in
  Printf.printf "day16a = %d\nday16b = %d\n" resulta resultb;;

day16 ();;
