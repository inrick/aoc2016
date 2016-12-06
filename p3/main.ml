open Core_kernel.Std
open Printf

let parse s = String.split_lines s
  |> List.map ~f:(fun t -> Scanf.sscanf t " %d %d %d" (fun x y z -> [x;y;z]))

let is_possible lst = match List.sort ~cmp:compare lst with
  | [x;y;z] -> x+y > z
  | _ -> assert false

let groupby3 lst =
  let rec go acc = function
    | x :: y :: z :: tl -> go ([x;y;z]::acc) tl
    | [] -> List.rev acc
    | _ -> assert false in
  go [] lst

let () =
  let input = In_channel.read_all "input.txt" |> parse in
  let possible = List.(filter input is_possible |> length) in
  printf "Part 1, possible: %d\n" possible;
  let input = List.(groupby3 input >>| transpose_exn |> concat) in
  let possible = List.(filter input is_possible |> length) in
  printf "Part 2, possible: %d\n" possible;
