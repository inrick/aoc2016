open Core_kernel.Std

let find_code s =
  let open Sequence in
  unfold_step ~init:0 ~f:(fun i ->
    let md5 = Digest.(s ^ string_of_int i |> string |> to_hex) in
    if String.slice md5 0 5 = "00000" then Step.Yield (md5.[5], i+1)
    else Step.Skip (i+1))
  |> Fn.flip take 8 |> to_list |> String.of_char_list

let find_code2 s =
  let open Sequence in
  unfold_step ~init:0 ~f:(fun i ->
    let md5 = Digest.(s ^ string_of_int i |> string |> to_hex) in
    if String.slice md5 0 5 = "00000" then
      try
        let ix = String.of_char md5.[5] |> Int.of_string in
        assert (0 <= ix && ix < 8);
        Step.Yield ((ix, md5.[6]), i+1)
      with _ -> Step.Skip (i+1)
    else Step.Skip (i+1))
  |> unfold_with ~init:Int_set.empty ~f:(fun found (i, chr) ->
    if not (Int_set.mem found i) then
      Step.Yield ((i, chr), Int_set.add found i)
    else Step.Skip found)
  |> Fn.flip take 8
  |> to_list
  |> fun xs -> String.init 8 (List.Assoc.find_exn xs)

let () =
  let input = "abbhdwsy" in
  let code = find_code input in
  print_endline code;
  let code2 = find_code2 input in
  print_endline code2;
