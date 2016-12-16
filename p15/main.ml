open Core_kernel.Std
open Printf
open Scanf

type disc = {positions : int; current : int} [@@deriving show]

let parse_disc s =
  sscanf s "Disc #%d has %d positions; at time=%d, it is at position %d." @@
    fun _ positions _ current -> {positions; current}

let next disc = {disc with current=(disc.current+1) mod disc.positions}

let solve discs =
  let open List in
  let rec go n ds =
    if for_all ds (fun d -> d.current = 0) then n
    else map ds next |> go (n+1) in
  mapi discs (fun n -> Fn.apply_n_times ~n:(n+1) next) |> go 0

let () =
  let discs = In_channel.read_lines "input.txt" |> List.map ~f:parse_disc in
  printf "%d\n" (solve discs);
  printf "%d\n" (solve (discs @ [{positions=11; current=0}]));
