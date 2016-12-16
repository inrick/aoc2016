open Core_kernel.Std

let generate len input =
  let open String in
  let step a =
    let b = rev a
      |> map ~f:(function '0' -> '1' | '1' -> '0' | _ -> assert false) in
    concat [a;"0";b] in
  let rec go s =
    if Int.(length s < len) then step s |> go
    else slice s 0 len in
  go input

let chksum input =
  let module B = BytesLabels in
  let step s =
    let len = String.length s lsr 1 in
    let buf = B.create len in
    for i = 0 to len-1 do
      B.set buf i (if s.[2*i] = s.[2*i+1] then '1' else '0')
    done;
    B.unsafe_to_string buf in
  let rec go s =
    if String.length s land 0b1 = 0 then step s |> go
    else s in
  go (step input)

let () =
  let input = "01111010110010011" in
  let data = generate 272 input in
  let chk = chksum data in
  print_endline chk;
