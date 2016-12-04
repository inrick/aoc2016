open Core_kernel.Std

module Keypad = struct
  type t = int array array
  type state = int * int
  type instr = U | D | L | R

  let keypad = [|[|1;2;3|];[|4;5;6|];[|7;8;9|]|]
  let initial_state = 1, 1 (* 5 on keypad *)

  let move (row,col) =
    let try_move pos = max 0 (min pos 2) in
    function
    | U -> try_move (row-1), col
    | D -> try_move (row+1), col
    | L -> row, try_move (col-1)
    | R -> row, try_move (col+1)

  let key (x, y) = keypad.(x).(y)
end

let lex_instrs lexbuf =
  let open Lexer in
  let open Keypad in
  List.map (tokens lexbuf) (function
    | UP -> U | DOWN -> D | LEFT -> L | RIGHT -> R | EOF -> assert false)

let parse s = Lexing.from_string s |> lex_instrs

let () =
  let input = In_channel.read_lines "input.txt" |> List.map ~f:parse in
  let code =
    List.fold_left input ~init:(Keypad.initial_state, []) ~f:(
      fun (state, code) instrs ->
        let end_state = List.fold_left instrs ~init:state ~f:Keypad.move in
        end_state, (end_state::code))
    |> snd |> List.rev_map ~f:Keypad.key in
  List.map code string_of_int |> String.concat |> print_endline
