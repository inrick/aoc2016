open Core_kernel.Std
open Printf

let abba s =
  let rec go = function
    | a :: b :: c :: d :: _ when a = d && b = c && a <> b -> true
    | _ :: tl -> go tl
    | [] -> false in
  go (String.to_list s)

let support_tls ip =
  let rec go i b = function
    | x :: xs when i land 0b1 = 0 -> go (i+1) (b || abba x) xs
    | x :: _ when abba x -> false
    | x :: xs -> go (i+1) b xs
    | [] -> b in
  String.split_on_chars ip ~on:['[';']'] |> go 0 false

let () =
  let input = In_channel.read_lines "input.txt" in
  List.(filter input support_tls |> length) |> printf "%d\n"
