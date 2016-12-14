open Core_kernel.Std
open Printf
open Scanf

let repeat n s =
  let open BytesLabels in
  let slen = String.length s in
  let buf = create (n * slen) in
  for i = 0 to length buf - 1 do
    set buf i s.[i mod slen]
  done;
  unsafe_to_string buf

let next_group s from =
  let rec go i = match s.[i] with
    | ')' -> i
    | _ -> go (i+1) in
  go from

let parse_group s start stop =
  String.slice s start stop |> fun t -> sscanf t "(%dx%d)" Tuple2.create

let process s =
  let open String in
  let next_group = next_group s in
  let decompress from (x,y) =
    let to_repeat = slice s from (from+x) in
    repeat y to_repeat in
  let slice_before i j = if Int.(i = j) then "" else slice s i j in
  let rec go acc last i = match s.[i] with
    | '(' ->
      let stop = next_group i in
      let x, y = parse_group s i (stop+1) in
      let before = slice_before last i in
      let decompressed = decompress (stop+1) (x,y) in
      go (decompressed :: before :: acc) (stop+1+x) (stop+1+x)
    | _ -> go acc last (i+1)
    | exception _ -> (slice_before last i :: acc) |> List.rev |> concat
  in
  go [] 0 0

let count2 s =
  let next_group = next_group s in
  let rec count n from until k =
    if from > until then k n
    else if from = until then k (n+1)
    else match s.[from] with
    | '(' ->
      let stop = next_group from in
      let x, y = parse_group s from (stop+1) in
      let end_next = stop + x in
      count 0 (stop+1) end_next @@ fun sub_count ->
        count n (end_next+1) until @@ fun rest ->
          k (y*sub_count + rest)
    | _ -> count (n+1) (from+1) until k in
  count 0 0 (String.length s - 1) ident

let () =
  let input = In_channel.read_all "input.txt" |> String.strip in
  let output = process input in
  (* print_endline output;*)
  printf "%d\n" (String.length output);
  printf "%d\n" (count2 input);
