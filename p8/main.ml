open Core_kernel.Std
open Printf

type instr =
  | Rect of int * int
  | Rot_col of int * int
  | Rot_row of int * int

let parse_instrs s =
  let open Lexer in
  let lexbuf = Lexing.from_string s in
  let rec go xs =
    let next instr = go (instr::xs) (read lexbuf) in
    function
    | EOF -> List.rev xs
    | RECT (x,y) -> next (Rect (x,y))
    | ROT_ROW (x,y) -> next (Rot_row (x,y))
    | ROT_COL (x,y) -> next (Rot_col (x,y)) in
  go [] (read lexbuf)

let print_mat =
  let open Array in
  iter ~f:(fun row ->
    iter row (fun b -> print_char (if b then '#' else '.')); print_newline ())

let init_matrix ~dimx ~dimy f =
  Array.(init dimx (fun i -> init dimy (fun j -> f i j)))

let sequence instrs =
  let open Array in
  let dimx, dimy = 6, 50 in
  let perform screen = function
    | Rect (a,b) ->
      mapi screen ~f:(fun i row ->
        if i < b then init dimy (fun j -> if j < a then true else row.(j))
        else row)
    | Rot_col (a,b) ->
      init_matrix ~dimx ~dimy (fun i j ->
        if j = a then screen.(Int.((i-b) % dimx)).(j)
        else screen.(i).(j))
    | Rot_row (a,b) ->
      let shifted = init dimy (fun j -> screen.(a).(Int.((j-b) % dimy))) in
      init dimx (fun i -> if i = a then shifted else screen.(i)) in
  let screen = make_matrix ~dimx ~dimy false in
  List.fold instrs ~init:screen ~f:perform

let () =
  let instrs = In_channel.read_all "input.txt" |> parse_instrs in
  let screen = sequence instrs in
  Array.(fold screen ~init:0 ~f:(fun i row -> i + count row ((=) true)))
    |> printf "%d\n";
  print_mat screen;
