open Core_kernel.Std

let find_code s =
  let open Sequence in
  unfold_step ~init:0 ~f:(fun i ->
    let md5 = Digest.(s ^ string_of_int i |> string |> to_hex) in
    if String.slice md5 0 5 = "00000" then Step.Yield (md5.[5], i+1)
    else Step.Skip (i+1))
  |> Fn.flip take 8 |> to_list |> String.of_char_list

let () =
  let code = find_code "abbhdwsy" in
  print_endline code
