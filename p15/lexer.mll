{
type token =
  | DISC of int * int * int * int
  | EOF
}

let int = ['0'-'9']+
let ws = [' ' '\t']+

rule read = parse
  | "Disc #" (int as num)
    " has " (int as positions)
    " positions; at time=" (int as time)
    ", it is at position " (int as current) "."
  { DISC (
    int_of_string num,
    int_of_string positions,
    int_of_string time,
    int_of_string current) }
  | eof { EOF }
  | _ { read lexbuf }
