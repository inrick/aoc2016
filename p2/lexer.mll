{
type token = UP | DOWN | LEFT | RIGHT | NEWLINE | EOF
}

rule read = parse
  | 'U' { UP }
  | 'D' { DOWN }
  | 'L' { LEFT }
  | 'R' { RIGHT }
  | '\n' | '\r' | "\r\n" { NEWLINE }
  | eof { EOF }
  | _ { read lexbuf }

{
let tokens lexbuf =
  let rec go xs = function
    | EOF -> List.rev (EOF::xs)
    | x -> go (x::xs) (read lexbuf) in
  go [] (read lexbuf)
}
