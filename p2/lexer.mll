{
type token = UP | DOWN | LEFT | RIGHT | EOF
}

rule read = parse
  | 'U' { UP } | 'D' { DOWN } | 'L' { LEFT } | 'R' { RIGHT }
  | _ { read lexbuf }
  | eof { EOF }

{
let tokens lexbuf =
  let rec go xs = function
    | EOF -> List.rev xs
    | x -> go (x::xs) (read lexbuf) in
  go [] (read lexbuf)
}
