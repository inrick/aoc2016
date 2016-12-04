open Core_kernel.Std

type instr = U | D | L | R

module type KEYPAD = sig
  type state

  val initial_state : state
  val move : state -> instr -> state
  val key : state -> string
end

module Keypad1 = struct
  type t = int array array
  type state = int * int

  let keypad = [|[|1;2;3|];[|4;5;6|];[|7;8;9|]|]
  let initial_state = 1, 1 (* 5 on keypad *)

  let move (row,col) =
    let try_move pos = max 0 (min pos 2) in
    function
    | U -> try_move (row-1), col
    | D -> try_move (row+1), col
    | L -> row, try_move (col-1)
    | R -> row, try_move (col+1)

  let key (x, y) = string_of_int keypad.(x).(y)
end

module Keypad2 = struct
  type t = char option array array
  type state = int * int

  let keypad = [|
    [|None;     None;     Some '1'; None;     None|];
    [|None;     Some '2'; Some '3'; Some '4'; None|];
    [|Some '5'; Some '6'; Some '7'; Some '8'; Some '9'|];
    [|None;     Some 'A'; Some 'B'; Some 'C'; None|];
    [|None;     None;     Some 'D'; None;     None|]
  |]

  let initial_state = 2, 0 (* 5 on keypad *)

  let move (row,col) =
    let try_move (a,b) =
      match keypad.(a).(b) with
      | Some _ -> a, b
      | None -> row, col
      | exception _ -> row, col in
    function
    | U -> try_move (row-1, col)
    | D -> try_move (row+1, col)
    | L -> try_move (row, col-1)
    | R -> try_move (row, col+1)

  let key (x, y) = match keypad.(x).(y) with
    | None -> failwith "invalid state"
    | Some k -> Char.to_string k
end

module Solver(K : KEYPAD) = struct
  let solve input =
    List.fold_left input ~init:(K.initial_state, []) ~f:(
      fun (state, code) instrs ->
        let end_state = List.fold_left instrs ~init:state ~f:K.move in
        end_state, (end_state::code))
    |> snd |> List.rev_map ~f:K.key |> String.concat
end

let parse s =
  let parse_row row = String.to_list row
    |> List.filter_map ~f:(function
      | 'U' -> Some U
      | 'D' -> Some D
      | 'L' -> Some L
      | 'R' -> Some R
      | _ -> None) in
  String.split_lines s |> List.map ~f:parse_row

let () =
  let input = In_channel.read_all "input.txt" |> parse in
  let module S1 = Solver(Keypad1) in
  let code1 = S1.solve input in
  print_endline code1;
  let module S2 = Solver(Keypad2) in
  let code2 = S2.solve input in
  print_endline code2
