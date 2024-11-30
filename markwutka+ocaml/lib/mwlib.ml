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

let range f t =
  let rec range1 f t acc =
    if f > t then
      List.rev acc
    else
      range1 (f+1) t (f::acc)
  in
  range1 f t [];;
