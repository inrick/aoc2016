{
open Parser
module L = Lexing

exception Syntax_error of string
}

let int = ['0'-'9']+
let ws = [' ' '\t']+
let nl = '\n' | '\r' | "\r\n"

rule read = parse
  | ws { read lexbuf }
  | ',' { read lexbuf }
  | nl { L.new_line lexbuf; read lexbuf }
  | int { INT (int_of_string (L.lexeme lexbuf)) }
  | 'L' { LEFT }
  | 'R' { RIGHT }
  | _ { raise (Syntax_error ("Unknown character: " ^ L.lexeme lexbuf)) }
  | eof { EOF }

{
let tokens lexbuf =
  let rec go xs = function
    | EOF -> List.rev (EOF::xs)
    | x -> go (x::xs) (read lexbuf) in
  go [] (read lexbuf)
}
