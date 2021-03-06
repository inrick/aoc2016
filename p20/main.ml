open Core_kernel.Std
open Scanf

let parse s = sscanf s "%d-%d" Tuple2.create

let find =
  let rec go from until = function
    | [] -> succ until
    | (x,y)::xs when succ until < x -> succ until
    | (x,y)::xs -> go (min from x) (max until y) xs in
  go 0 0

let count =
  let rec go n from until = function
    | [] -> n
    | (x,y)::xs when succ until < x ->
      go (n + (x - succ until)) from (max until y) xs
    | (x,y)::xs -> go n (min from x) (max until y) xs in
  go 0 0 0

let () =
  let input = In_channel.read_lines "input.txt"
    |> List.map ~f:parse
    |> List.sort ~cmp:(fun (a,_) (b,_) -> compare a b) in
  let smallest = find input in
  printf "%d\n" smallest;
  let allowed = count input in
  printf "%d\n" allowed;
