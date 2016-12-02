open Core_kernel.Std
open Printf

module Coord = struct
  type t = int * int

  let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

  let (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
end

type orient = N | W | E | S
type dir = L of int | R of int

let directions lexbuf =
  let open Lexer in
  let rec go xs = function
    | EOF -> List.rev xs
    | LEFT i -> go (L i::xs) (read lexbuf)
    | RIGHT i -> go (R i::xs) (read lexbuf)
  in go [] (read lexbuf)

let parse s = Lexing.from_string s |> directions

let move current_pos orient dir =
  let new_orient, diff = match orient, dir with
  | N, L i | S, R i -> W, (-i, 0)
  | N, R i | S, L i -> E, (i, 0)
  | W, L i | E, R i -> S, (0, -i)
  | W, R i | E, L i -> N, (0, i) in
  new_orient, Coord.(current_pos + diff)

let navigate start_orient start_pos dirs =
  let _, end_pos = List.fold_left dirs ~init:(start_orient, start_pos) ~f:(
    fun (orient, pos) dir -> move pos orient dir) in
  end_pos

let () =
  let dirs = In_channel.read_all "input.txt" |> parse in
  let start_pos = 0, 0 in
  let end_pos = navigate N start_pos dirs in
  let x, y = end_pos in
  printf "End coordinate: (%d, %d), distance: %d\n" x y
    (Coord.dist start_pos end_pos)
