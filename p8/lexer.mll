{
type token =
  | RECT of int * int
  | ROT_COL of int * int
  | ROT_ROW of int * int
  | EOF
}

let int = ['0'-'9']+
let ws = [' ' '\t']+

rule read = parse
  | "rect" ws (int as x) 'x' (int as y)
  { RECT (int_of_string x, int_of_string y) }
  | "rotate column x=" (int as x) ws "by" ws (int as y)
  { ROT_COL (int_of_string x, int_of_string y) }
  | "rotate row y=" (int as x) ws "by" ws (int as y)
  { ROT_ROW (int_of_string x, int_of_string y) }
  | eof { EOF }
  | _ { read lexbuf }
