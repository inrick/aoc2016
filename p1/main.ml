open Core_kernel.Std

type dir = Left of int | Right of int

let parse s = Lexing.from_string s |> Lexer.tokens |> List.filter_map ~f:(
  function
  | Lexer.LEFT x -> Some (Left x)
  | Lexer.RIGHT x -> Some (Right x)
  | Lexer.EOF -> None)

let () =
  assert (parse "R1, L1" = [Right 1; Left 1]);
  print_endline "lol"
