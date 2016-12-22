{
type token =
  | SWAP_POS of int * int
  | SWAP_CHAR of char * char
  | ROT_LEFT of int
  | ROT_RIGHT of int
  | ROT_REL of char
  | REVERSE of int * int
  | MOVE of int * int
  | EOF

let int = int_of_string
}

let int = ['0'-'9']+
let char = ['a'-'z']

rule read = parse
  | "swap position " (int as x) " with position " (int as y)
  { SWAP_POS (int x, int y) }
  | "swap letter " (char as x) " with letter " (char as y)
  { SWAP_CHAR (x, y) }
  | "rotate left " (int as x) " step" 's'?
  { ROT_LEFT (int x) }
  | "rotate right " (int as x) " step" 's'?
  { ROT_RIGHT (int x) }
  | "rotate based on position of letter " (char as x)
  { ROT_REL x }
  | "reverse positions " (int as x) " through " (int as y)
  { REVERSE (int x, int y) }
  | "move position " (int as x) " to position " (int as y)
  { MOVE (int x, int y) }
  | eof { EOF }
  | _ { read lexbuf }
