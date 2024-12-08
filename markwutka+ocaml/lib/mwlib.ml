let read_file filename =
  In_channel.with_open_text filename In_channel.input_lines

let read_file_as_string filename =
  In_channel.with_open_text filename In_channel.input_all

let split_groups l =
    let f (acc,lists) l =
        if String.length l = 0 then
            ([], acc::lists)
        else
            (l::acc, lists)
    in
      List.rev (""::l) |>
      List.fold_left f ([],[])
      |> snd;;

let pair x y = (x, y)

let product l1 l2 =
  let prod1 l x = List.map (pair x) l in                             
  List.concat_map (prod1 l2) l1

let rec all f l =
  match l with
  | [] -> true
  | x :: rest -> if not (f x) then false else all f rest

let rec any f l =
  match l with
  | [] -> false
  | x :: rest -> if f x then true else any f rest

let range f t =
  let rec range1 f t acc =
    if f >= t then
      List.rev acc
    else
      range1 (f+1) t (f::acc)
  in
  range1 f t [];;

let string_to_list s = List.of_seq (String.to_seq s)

let rec gcd a b =
  let m = a mod b in
  if m == 0 then b else gcd b m
