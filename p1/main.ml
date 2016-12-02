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

let navigate start_orient start_coord dirs =
  let move orient dir = match orient, dir with
  | N, L i | S, R i -> W, (-i, 0)
  | N, R i | S, L i -> E, (i, 0)
  | W, L i | E, R i -> S, (0, -i)
  | W, R i | E, L i -> N, (0, i) in
  let _, end_coord = List.fold_left dirs ~init:(start_orient, start_coord) ~f:(
    fun (orient, coord) dir ->
      let new_orient, coord_diff = move orient dir in
      new_orient, Coord.(coord + coord_diff)) in
  end_coord

let () =
  let dirs = In_channel.read_all "input.txt" |> parse in
  let start_coord = 0, 0 in
  let end_coord = navigate N start_coord dirs in
  let x, y = end_coord in
  printf "End coordinate: (%d, %d), distance: %d\n" x y
    (Coord.dist start_coord end_coord)
