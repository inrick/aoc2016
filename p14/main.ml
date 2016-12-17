open Core_kernel.Std
open Printf

module M = Map.Make(Char)

exception Found of char

let find_triplet s =
  try
    for i = 0 to String.length s - 3 do
      if s.[i] = s.[i+1] && s.[i+1] = s.[i+2] then
        raise (Found s.[i]);
    done;
    None
  with Found c -> Some c

let quintuplet s c =
  try
    for i = 0 to String.length s - 5 do
      if s.[i] = c && s.[i+1] = c && s.[i+2] = c && s.[i+3] = c && s.[i+4] = c
      then raise Not_found;
    done;
    false
  with Not_found -> true

let solve salt =
  let open Sequence in
  let next i m = M.filter_mapi m (fun ~key ~data ->
    match List.filter data (fun j -> i-j < 1000) with
    | [] -> None
    | xs -> Some xs) in
  unfold_step ~init:(0, M.empty) ~f:(fun (i, m) ->
    let md5 = Digest.(salt ^ string_of_int i |> string |> to_hex) in
    let add_triplet m = match find_triplet md5 with
      | Some c ->
        M.add m ~key:c ~data:(i :: match M.find m c with
          | Some keys -> keys
          | None -> [])
      | None -> m in
    match M.to_alist m |> List.find ~f:(fun (c, _) -> quintuplet md5 c) with
    | None -> Step.Skip (i+1, add_triplet m |> next i)
    | Some (c, keys) ->
      Step.Yield (of_list keys, (i+1, M.remove m c |> add_triplet |> next i)))
  |> join
  (* as sequence is not emitted in order, take some extra and sort after *)
  |> Fn.flip take 80
  |> to_list
  |> List.sort ~cmp:Int.compare
  |> Fn.flip List.nth_exn 63

let () =
  let salt = "jlmsuwbz" in
  printf "%d\n" (solve salt);
