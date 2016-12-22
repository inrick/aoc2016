open Core_kernel.Std

type instr =
  | Swap_pos of int * int
  | Swap_char of char * char
  | Rot_left of int
  | Rot_right of int
  | Rot_rel of char
  | Reverse of int * int
  | Move of int * int
  [@@deriving show]

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

let scramble s instrs =
  let open String in
  let len = length s in
  let rec go s = function
    | [] -> s
    | i::is ->
      match i with
      | Swap_pos (x, y) ->
        let t = copy s in
        t.[x] <- s.[y];
        t.[y] <- s.[x];
        go t is
      | Swap_char (x, y) ->
        go (map s
          (fun c -> Char.(if c = x then y else if c = y then x else c))) is
      | Rot_left x ->
        go (init len (fun i -> s.[Int.((i+x) % len)])) is
      | Rot_right x ->
        go (init len (fun i -> s.[Int.((i-x) % len)])) is
      | Rot_rel x ->
        let rot = match index s x with
          | None -> assert false
          | Some i -> 1 + i + if Int.(i >= 4) then 1 else 0 in
        go s (Rot_right rot::is)
      | Reverse (x, y) ->
        let t = copy s in
        for i = 0 to y-x do
          t.[x+i] <- s.[y-i]
        done;
        go t is
      | Move (x, y) ->
        let open Int in
        let t =
          if x < y then init len (fun i ->
            if i = y then s.[x]
            else if x <= i && i < y then s.[i+1]
            else s.[i])
          else init len (fun i ->
            if i = y then s.[x]
            else if y < i && i <= x then s.[i-1]
            else s.[i]) in
        go t is in
  go s instrs

let () =
  let input = In_channel.read_all "input.txt" |> parse in
  let scrambled = scramble "abcdefgh" input in
  print_endline scrambled;
