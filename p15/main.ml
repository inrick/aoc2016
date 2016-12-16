open Core_kernel.Std
open Printf

type disc = {positions : int; current : int} [@@deriving show]

let parse_discs s =
  let open Lexer in
  let lexbuf = Lexing.from_string s in
  let rec go xs = function
    | EOF -> List.rev xs
    | DISC (num,positions,time,current) ->
      go ({positions=positions;current=current}::xs) (read lexbuf) in
  go [] (read lexbuf)

let next disc = {disc with current=(disc.current+1) mod disc.positions}

let solve discs =
  let open List in
  let rec go n ds =
    if for_all ds (fun d -> d.current = 0) then n
    else map ds next |> go (n+1) in
  mapi discs (fun n -> Fn.apply_n_times ~n:(n+1) next) |> go 0

let () =
  let discs = In_channel.read_all "input.txt" |> parse_discs in
  printf "%d\n" (solve discs);
  printf "%d\n" (solve (discs @ [{positions=11; current=0}]));
