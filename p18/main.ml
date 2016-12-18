open Core_kernel.Std
open Printf

let gen s rows =
  let cols = String.length s in
  let is_trap row i = if i < 0 || cols <= i then false else row.[i] = '^' in
  let next prev =
    let is_trap = is_trap prev in
    String.init cols (fun i ->
      if (is_trap (i-1) && is_trap i && not (is_trap (i+1)))
      || (not (is_trap (i-1)) && is_trap i && is_trap (i+1))
      || (is_trap (i-1) && not (is_trap i) && not (is_trap (i+1)))
      || (not (is_trap (i-1)) && not (is_trap i) && is_trap (i+1))
      then '^' else '.') in
  let rec go n acc prev =
    if n = rows then List.rev acc
    else let xs = next prev in go (n+1) (xs::acc) xs in
  go 1 [s] s

let count_safe board =
  let open List in
  map board ~f:(String.count ~f:((=) '.')) |> fold ~init:0 ~f:(+)

let () =
  let input =
    ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."
  in
  let board = gen input 40 in
  let safe = count_safe board in
  printf "%d\n" safe;
