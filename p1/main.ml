open Core_kernel.Std

type dir = Left of int | Right of int

let directions lexbuf =
  let open Lexer in
  let rec go xs = function
    | EOF -> List.rev xs
    | LEFT i -> go (Left i::xs) (read lexbuf)
    | RIGHT i -> go (Right i::xs) (read lexbuf)
  in go [] (read lexbuf)

let parse s = Lexing.from_string s |> directions

let () =
  assert (parse "R1, L1" = [Right 1; Left 1]);
  print_endline "lol"
