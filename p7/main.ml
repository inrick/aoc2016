open Core_kernel.Std
open Printf

module S = Set.Make(String)

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

let find_abas s =
  let rec go acc = function
    | a :: b :: c :: tl when a = c && a <> b -> go ([a;b;a] :: acc) (b::c::tl)
    | _ :: tl -> go acc tl
    | [] -> List.(acc >>| String.of_char_list |> rev) in
  go [] (String.to_list s)

let any_bab abas s =
  let rec go i = function
    | a :: b :: c :: tl when a = c && a <> b ->
      S.mem abas (String.of_char_list [b;a;b]) || go (i+1) (b::c::tl)
    | _ :: tl -> go (i+1) tl
    | [] -> false in
  go 0 (String.to_list s)

let support_ssl ip =
  let open List in
  let parts = String.split_on_chars ip ~on:['[';']'] in
  let supernets, hypernets =
    mapi parts Tuple2.create
    |> partition_tf ~f:(fun (i,_) -> i land 0b1 = 0)
    |> fun (x,y) -> (x >>| snd, y >>| snd) in
  let abas = supernets >>= find_abas |> S.of_list in
  find hypernets (any_bab abas) |> Option.is_some

let () =
  let open List in
  let input = In_channel.read_lines "input.txt" in
  filter input support_tls |> length |> printf "%d\n";
  filter input support_ssl |> length |> printf "%d\n";
