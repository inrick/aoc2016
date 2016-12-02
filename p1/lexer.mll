{
module L = Lexing

type token = LEFT of int | RIGHT of int | EOF

exception Syntax_error of string
}

let int = ['0'-'9']+
let ws = [' ' '\t']+
let nl = '\n' | '\r' | "\r\n"

rule read = parse
  | ws { read lexbuf }
  | ',' { read lexbuf }
  | nl { L.new_line lexbuf; read lexbuf }
  | 'L' (int as n) { LEFT (int_of_string n) }
  | 'R' (int as n) { RIGHT (int_of_string n) }
  | _ { raise (Syntax_error ("Unknown character: " ^ L.lexeme lexbuf)) }
  | eof { EOF }
