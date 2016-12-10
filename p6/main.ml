open Core_kernel.Std

let solve xs =
  let open List in
  xs >>| String.to_list
    |> transpose_exn
    >>| (fun ys ->
      sort ~cmp:(fun a b -> count ys ((=) b) - count ys ((=) a)) ys)
    >>| hd
    >>= Option.to_list
    |> String.of_char_list

let solve2 xs =
  let open List in
  xs >>| String.to_list
    |> transpose_exn
    >>| (fun ys ->
      sort ~cmp:(fun a b -> count ys ((=) a) - count ys ((=) b)) ys)
    >>| hd
    >>= Option.to_list
    |> String.of_char_list

let () =
  let input = In_channel.read_lines "input.txt" in
  solve input |> print_endline;
  solve2 input |> print_endline;
