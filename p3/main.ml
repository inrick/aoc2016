open Core_kernel.Std
open Printf

let parse s = String.split_lines s
  |> List.map ~f:(fun t -> Scanf.sscanf t " %d %d %d" (fun x y z -> [x;y;z]))

let is_possible lst = match List.sort ~cmp:compare lst with
  | [x;y;z] -> x+y > z
  | _ -> assert false

let transpose3 = function (* List.transpose_exn already does this in general *)
  | [[x1;y1;z1];[x2;y2;z2];[x3;y3;z3]] ->
      [[x1;x2;x3];[y1;y2;y3];[z1;z2;z3]]
  | _ -> assert false

let () =
  let input = In_channel.read_all "input.txt" |> parse in
  let possible = List.(filter input is_possible |> length) in
  printf "Part 1, possible: %d\n" possible;
  let input =
    List.(groupi input (fun i _ _ -> i mod 3 = 0) >>| transpose3 |> concat) in
  let possible = List.(filter input is_possible |> length) in
  printf "Part 2, possible: %d\n" possible;
