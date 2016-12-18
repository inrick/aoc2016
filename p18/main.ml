open Core_kernel.Std
open Printf

let gen s rows =
  let cols = String.length s in
  let is_safe prev i = i < 0 || cols <= i || prev.[i] = '.' in
  let next prev =
    let is_safe = is_safe prev in
    String.init cols (fun i ->
      match is_safe (i-1), is_safe i, is_safe (i+1) with
      | false, false, true | true, false, false
      | false, true,  true | true, true,  false -> '^'
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
