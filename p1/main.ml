open Core_kernel.Std

module Coord = struct
  type t = int * int

  let dist (a, b) (c, d) = abs (a - c) + abs (b - d)
  let (+) (a, b) (c, d) = (a + c, b + d)
  let sum = List.fold_left ~init:(0,0) ~f:(+)
end

type direction = N | W | E | S
type move = direction * int
type step = L of int | R of int

let lex_steps lexbuf =
  let open Lexer in
  let rec go xs = function
    | EOF -> List.rev xs
    | LEFT n -> go (L n::xs) (read lexbuf)
    | RIGHT n -> go (R n::xs) (read lexbuf)
  in go [] (read lexbuf)

let parse s = Lexing.from_string s |> lex_steps

let move dir step = match dir, step with
  | N, L n | S, R n -> W, n
  | N, R n | S, L n -> E, n
  | W, L n | E, R n -> S, n
  | W, R n | E, L n -> N, n

let move_to_coord = function
  | N, n -> (0, n)
  | W, n -> (-n, 0)
  | E, n -> (n, 0)
  | S, n -> (0, -n)

let moves start_dir steps =
  let rec go (dir,_) moves = function
    | [] -> List.rev moves
    | step::steps -> let next = move dir step in go next (next::moves) steps in
  go (start_dir,0) [] steps

let () =
  let open Printf in
  let steps = In_channel.read_all "input.txt" |> parse in
  let end_pos = moves N steps |> List.map ~f:move_to_coord |> Coord.sum in
  printf "End coordinate: (%d, %d), distance: %d\n"
    (fst end_pos) (snd end_pos) (Coord.dist (0,0) end_pos)
