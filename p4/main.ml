open Core_kernel.Std
open Printf

type room = {name : string; id: int; chk : string}

let parse s =
  let open String in
  let name = slice s 0 (-10) |> filter ~f:(Char.(<>) '-') in
  let id = slice s (-10) (-7) |> int_of_string in
  let chk = slice s (-6) (-1) in
  {name; id; chk}

let validate r =
  let open List in
  let chk =
    String.to_list r.name
    |> sort ~cmp:Char.compare
    |> group ~break:(<>)
    |> sort ~cmp:(fun xs ys -> length ys - length xs)
    |> fun xs -> take xs 5
    |> map ~f:hd_exn
    |> String.of_char_list in
  chk = r.chk

let () =
  let input = In_channel.read_lines "input.txt" in
  let rooms = List.map input parse in
  let sum =
    List.filter_map rooms ~f:(fun r -> if validate r then Some r.id else None)
    |> List.fold_left ~init:0 ~f:(+) in
  printf "%d\n" sum
