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
  | 'L' (int as i) { LEFT (int_of_string i) }
  | 'R' (int as i) { RIGHT (int_of_string i) }
  | _ { raise (Syntax_error ("Unknown character: " ^ L.lexeme lexbuf)) }
  | eof { EOF }
