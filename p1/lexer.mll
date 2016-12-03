{
type token = LEFT of int | RIGHT of int | EOF
}

let int = ['0'-'9']+

rule read = parse
  | 'L' (int as n) { LEFT (int_of_string n) }
  | 'R' (int as n) { RIGHT (int_of_string n) }
  | _ { read lexbuf }
  | eof { EOF }
