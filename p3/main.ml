open Core_kernel.Std
open Printf

let parse s = String.split_lines s
  |> List.map ~f:(fun t ->
      Scanf.sscanf t " %d %d %d" (fun x y z ->
        match List.sort ~cmp:compare [x;y;z] with
        | [x;y;z] -> x,y,z
        | _ -> assert false))

let is_possible (x,y,z) = x+y > z

let () =
  let input = In_channel.read_all "input.txt" |> parse in
  let possible = List.filter input is_possible |> List.length in
  printf "Possible: %d\n" possible
