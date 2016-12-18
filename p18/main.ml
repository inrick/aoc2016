open Core_kernel.Std
open Printf

let gen s rows =
  let cols = String.length s in
  let segment prev i =
    if i < 0 || cols <= i then '.' else prev.[i] in
  let next prev =
    String.init cols (fun i ->
      match String.init 3 (fun j -> segment prev (j+i-1)) with
      | "^^." | ".^^" | "^.." | "..^" -> '^'
      | _ -> '.') in
  let rec go n acc prev =
    if n = rows then List.rev acc
    else let r = next prev in go (n+1) (r::acc) r in
  go 1 [s] s

let count_safe board =
  List.(map board ~f:(String.count ~f:((=) '.')) |> fold ~init:0 ~f:(+))

let () =
  let input =
    ".^^^.^.^^^.^.......^^.^^^^.^^^^..^^^^^.^.^^^..^^.^.^^..^.^..^^...^.^^.^^^...^^.^.^^^..^^^^.....^...."
  in
  let board = gen input 40 in
  let safe = count_safe board in
  printf "%d\n" safe;
  let safe2 = gen input 400000 |> count_safe in
  printf "%d\n" safe2;
