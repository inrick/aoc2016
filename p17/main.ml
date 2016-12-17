open Core_kernel.Std
open Printf

(*
 (0,0)
   #########
   #S| | | #
   #-#-#-#-#
   # | | | #
   #-#-#-#-#
   # | | | #
   #-#-#-#-#
   # | | |
   ####### V
         (3,-3)
 *)

module S = Set.Make(String)

type dir = U | D | L | R
type state = {pos : int * int; path : string}

let show_dir = function U -> "U" | D -> "D" | L -> "L" | R -> "R"

let move (x,y) = function
  | U -> x, y+1
  | D -> x, y-1
  | L -> x-1, y
  | R -> x+1, y

let valid_pos (x,y) = 0 <= x && x <= 3 && -3 <= y && y <= 0

let open_doors s =
  let dir_of_int d : dir = Obj.magic d in (* :) *)
  let door i = match s.[i] with
    | 'b' | 'c' | 'd' | 'e' | 'f' -> Some (dir_of_int i)
    | _ -> None in
  List.(range 0 4 |> filter_map ~f:door)

let paths target passcode =
  let open List in
  let rec go found st =
    if st.pos = target then S.add found st.path
    else
      let room = Digest.(passcode ^ st.path |> string |> to_hex) in
      let possible = open_doors room
        >>| (fun dir -> {pos=move st.pos dir; path=st.path ^ (show_dir dir)})
        |> filter ~f:(fun st -> valid_pos st.pos) in
      fold possible ~init:found ~f:go in
  go S.empty {pos=0,0; path=""}

let () =
  let open List in
  let input = "njfxhljp" in
  let ps = paths (3,-3) input
    |> S.to_list
    |> sort ~cmp:(fun x y -> String.(length x - length y)) in
  let path1 = hd_exn ps in
  print_endline path1;
  let path2 = last_exn ps in
  printf "%d\n" (String.length path2);
