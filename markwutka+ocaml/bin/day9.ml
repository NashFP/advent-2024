open Advent_lib;;

(* Day 9 was about optimizing a disk given a list of alternating block
   sizes and free space sizes, and then computing a checksum. The tricky
   part is that in optimizing it, you pull blocks from the rightmost
   part of the disk to fill in free space going from left to right.

   For both parts, I am avoiding making an actual image of the disk, and
   instead I am computing the checksum from the list of blocks and free
   spaces. The first key part is that the split_list function partitions
   the original list into a list of blocks and a list of free spaces
   and it computes the location on the disk where each item starts.

   In both cases, I start from the rightmost disk sector. For part a,
   I fill in the first free space with the contents of the block. If the
   free space is the same length as the block, I compute the checksum
   for that block as if its position is in the free space, and then
   remove both the block and the free space from their respective
   lists.
   
   If the block is longer than the free space, I remove the free space from
   the free list, and shorten the block by the length of the free amount
   If the block is shorter than the free space, I remove the block from
   the block list, and shorten the free space by the block length.
   Once the starting pos of the free space is past the current block,
   we just need to do the computation with the remaining blocks because
   they won't get moved as there was no free space available to move
   them to.

   For part b, the strategy is similar, except that instead of splitting
   blocks, you look for a free space that can contain the entire block.
   I compute an index of the first available free space, and if it is
   to the left of the block, I remove that space from the free list
   and remove the block from the list. If there is no match, I leave
   the block in place, add it to the checksum, and leave the free list
   as-is.
 *)

let split_list lst =
  let rec loop is_left lst left_acc right_acc pos block_num =
    match lst with
    | [] -> (List.rev left_acc, List.rev right_acc)
    | x :: rest -> if is_left then
                     loop false rest ((x, pos, block_num) :: left_acc)
                       right_acc (pos+x) (block_num+1)
                   else
                     loop true rest left_acc ((x,pos) :: right_acc)
                       (pos+x) block_num
  in
  loop true lst [] [] 0 0;;

(* To compute the sum of block values with a particular length at a particular
   position, it would be block num * (pos + pos+1 + pos+2 ... pos+len-1)
   This can be factored into block num * ((pos*len) + sum 1..(len-1)),
   or num * (pos * len + (len * (len-1)) / 2)
 *)          
let block_sum_range pos len num =
  num * (len * pos + len * (len - 1) / 2)

let compute_checksum_a block_sizes free_sizes =
  let rec loop block_sizes free_sizes checksum =
    match block_sizes with
    | [] -> checksum
    | (block_len, block_pos, block_num) :: block_rest ->
       match free_sizes with
       | [] -> loop block_rest free_sizes
                 (checksum + block_sum_range block_pos block_len block_num)
       | (free_len, free_pos) :: free_rest ->
          if free_pos > block_pos then
          (* If all the free spaces are to the right of the current block,
             clear out the free list and just process the rest of the blocks *)
            loop block_rest []
              (checksum + block_sum_range block_pos block_len block_num)
          else if free_len == block_len then
            (* If the sizes exactly match, remove both the block and the free
               space from their lists, and compute the block checksum as
               if it is in the free space *)
            loop block_rest free_rest
              (checksum + block_sum_range free_pos block_len block_num)
          else if free_len < block_len then
            (* If the free size is smaller than the block, remove the
               free size from the list and shorten the block by the
               size of the free size *)
            loop ((block_len - free_len, block_pos, block_num)::block_rest)
              free_rest (checksum + block_sum_range free_pos free_len block_num)
          else
            (* If the free size is larger than the block, remove the
               block from the list, and shorten the free space by the
               size of the block *)
            loop block_rest
              ((free_len - block_len, free_pos + block_len) :: free_rest)
              (checksum + block_sum_range free_pos block_len block_num)
  in
  loop (List.rev block_sizes) free_sizes 0

(** Return the index of the first item in l where f returns true *) 
let index_of f l =
  let rec loop l pos =
    match l with
    | [] -> ((-1,-1), -1)
    | x :: rest -> if f x then
                     (x, pos)
                   else
                     loop rest (pos+1)
  in
  loop l 0

(** Drop the item at position index from the list *)
let drop_at index l =
  let rec loop l pre pos =
    match l with
    | [] -> l
    | x :: rest -> if pos == index then List.rev_append pre rest
                   else loop rest (x::pre) (pos+1)
  in
  loop l [] 0

(** Shorten the free list item at position index by the specified amount *)
let shorten_at index amount l =
  let rec loop l pre pos =
    match l with
    | [] -> l
    | (free_len, free_pos) :: free_rest ->
       if pos == index then
         List.rev_append pre ((free_len-amount, free_pos+amount)::free_rest)
       else
         loop free_rest ((free_len, free_pos) :: pre) (pos+1)
  in
  loop l [] 0

let compute_checksum_b block_sizes free_sizes =
  let can_fit block_size (size,_) = size >= block_size in
  let rec loop block_sizes free_sizes checksum =
    match block_sizes with
    | [] -> checksum
    | (block_len, block_pos, block_num) :: block_rest ->
       if List.is_empty free_sizes then
         (* If there are no free spaces left, just process the blocks *)
         loop block_rest free_sizes
           (checksum + block_sum_range block_pos block_len block_num)
       else
         (* Find the first free space at least as large as the block *)
         let ((free_len, free_pos), idx) =
           index_of (can_fit block_len) free_sizes in

         (* Make sure the free space is to the left of the block *)
         if idx >= 0 && free_pos < block_pos then
           if free_len == block_len then
             (* If the sizes match exactly, from the free space from the list *)
             loop block_rest (drop_at idx free_sizes)
               (checksum + block_sum_range free_pos block_len block_num)
           else
             (* Otherwise, shorten the free space in-place *)
             loop block_rest (shorten_at idx block_len free_sizes)
               (checksum + block_sum_range free_pos block_len block_num)
         else
           (* If no matching free space is found, leave the block in place*)
           loop block_rest free_sizes
             (checksum + block_sum_range block_pos block_len block_num)
  in
  loop (List.rev block_sizes) free_sizes 0          

let day9 () =
  let line = String.trim (Mwlib.read_file_as_string "data/day9.txt") in
  let length_list = List.map (fun ch -> Char.code ch - Char.code '0')
                      (Mwlib.string_to_list line) in
  let (block_sizes, free_sizes) = split_list length_list in
  let resulta = compute_checksum_a block_sizes free_sizes in
  let resultb = compute_checksum_b block_sizes free_sizes in
  Printf.printf "day9a = %d\n day9b = %d\n" resulta resultb;;

day9 ();;
