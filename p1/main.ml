open Core_kernel.Std

let () =
  let parse s = Lexing.from_string s |> Parser.dirs Lexer.read in
  assert (parse "R1, L1" = [Dirs.Right 1; Dirs.Left 1]);
  print_endline "lol"
