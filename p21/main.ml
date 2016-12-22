open Core_kernel.Std

type instr =
  | Swap_pos of int * int
  | Swap_char of char * char
  | Rot_left of int
  | Rot_right of int
  | Rot_rel of char
  | Reverse of int * int
  | Move of int * int

let parse s =
  let open Lexer in
  let lexbuf = Lexing.from_string s in
  let rec go xs =
    let next instr = go (instr::xs) (read lexbuf) in
    function
    | EOF -> List.rev xs
    | SWAP_POS (x, y) -> next (Swap_pos (x, y))
    | SWAP_CHAR (x, y) -> next (Swap_char (x, y))
    | ROT_LEFT x -> next (Rot_left x)
    | ROT_RIGHT x -> next (Rot_right x)
    | ROT_REL x -> next (Rot_rel x)
    | REVERSE (x, y) -> next (Reverse (x, y))
    | MOVE (x, y) -> next (Move (x, y)) in
  go [] (read lexbuf)

let rec eval s =
  let module B = BytesLabels in
  let len = String.length s in
  function
  | Swap_pos (x, y) ->
    let t = B.of_string s in
    B.set t x s.[y];
    B.set t y s.[x];
    B.to_string t
  | Swap_char (x, y) ->
    String.map s (fun c -> if c = x then y else if c = y then x else c)
  | Rot_left x -> String.init len (fun i -> s.[Int.((i+x) % len)])
  | Rot_right x -> String.init len (fun i -> s.[Int.((i-x) % len)])
  | Rot_rel x ->
    let rot = match String.index s x with
      | None -> assert false
      | Some i -> 1 + i + if i >= 4 then 1 else 0 in
    eval s (Rot_right rot)
  | Reverse (x, y) ->
    let t = B.of_string s in
    for i = 0 to y-x do
      B.set t (x+i) s.[y-i]
    done;
    B.to_string t
  | Move (x, y) ->
    if x < y then String.init len (fun i ->
      if i = y then s.[x]
      else if x <= i && i < y then s.[i+1]
      else s.[i])
    else String.init len (fun i ->
      if i = y then s.[x]
      else if y < i && i <= x then s.[i-1]
      else s.[i])

let scramble s instrs = List.fold instrs ~init:s ~f:eval

let () =
  let instrs = In_channel.read_all "input.txt" |> parse in
  let scrambled = scramble "abcdefgh" instrs in
  print_endline scrambled;
