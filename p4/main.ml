open Core_kernel.Std
open Printf

type room = {name : string; id: int; chk : string}

let parse s =
  let open String in
  let name = slice s 0 (-11) in
  let id = slice s (-10) (-7) |> int_of_string in
  let chk = slice s (-6) (-1) in
  {name; id; chk}

let validate r =
  let open List in
  let chk =
    String.(filter r.name (Char.(<>) '-') |> to_list)
    |> sort ~cmp:Char.compare
    |> group ~break:(<>)
    |> sort ~cmp:(fun xs ys -> length ys - length xs)
    |> fun xs -> take xs 5
    |> map ~f:hd_exn
    |> String.of_char_list in
  chk = r.chk

let alphabet = "abcdefghijklmnopqrstuvwxyz"

let rotate n c =
  if c = '-' then ' '
  else alphabet.[(String.index_exn alphabet c + n) mod 26]

let decrypt r = {r with name = String.map r.name (rotate r.id)}

let () =
  let input = In_channel.read_lines "input.txt" in
  let rooms = List.map input parse in
  let sum =
    List.filter_map rooms ~f:(fun r -> if validate r then Some r.id else None)
    |> List.fold_left ~init:0 ~f:(+) in
  printf "%d\n" sum;
  let candidate = List.(rooms >>| decrypt |> find_exn ~f:(fun r ->
    String.substr_index r.name ~pattern:"north" |> Option.is_some)) in
  printf "%d\n" candidate.id
