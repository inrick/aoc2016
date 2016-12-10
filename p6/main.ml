open Core_kernel.Std

let solve xs solver =
  let open List in
  xs >>| String.to_list
    |> transpose_exn
    >>| solver
    >>| hd
    >>= Option.to_list
    |> String.of_char_list

let solver1 chrs =
  List.(sort ~cmp:(fun a b -> count chrs ((=) b) - count chrs ((=) a)) chrs)

let solver2 chrs =
  List.(sort ~cmp:(fun a b -> count chrs ((=) a) - count chrs ((=) b)) chrs)

let () =
  let input = In_channel.read_lines "input.txt" in
  solve input solver1 |> print_endline;
  solve input solver2 |> print_endline;
