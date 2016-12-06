open Core_kernel.Std

module List = struct
  include List

  let scan_left xs ~init ~f =
    fold_left xs ~init:(init, []) ~f:(fun (x, zs) y ->
      let z = f x y in z, z::zs) |> snd |> rev

  let each_pair xs ~f =
    let rec go acc = function
      | x::y::tl -> go (f x y::acc) (y::tl)
      | [_] | [] -> List.rev acc in
    go [] xs
end

module Coord = struct
  type t = int * int

  let dist (a, b) (c, d) = abs (a - c) + abs (b - d)
  let (+) (a, b) (c, d) = (a + c, b + d)
  let sum = List.fold_left ~init:(0,0) ~f:(+)
  let show (a, b) = Printf.sprintf "(%d, %d)" a b
end

module Visited = struct
  type t = (Coord.t, unit) Hashtbl.t

  exception Found of Coord.t

  let create () = Hashtbl.create ~hashable:Hashtbl.Poly.hashable ()

  let range p q =
    let points a b =
      let xs =
        List.range ~start:`inclusive ~stop:`inclusive (min a b) (max a b) in
      List.drop (if a <= b then xs else List.rev xs) 1 in
    match p, q with
    | (a, b), (c, d) when a = c -> List.map (points b d) (fun x -> a, x)
    | (a, b), (c, d) when b = d -> List.map (points a c) (fun x -> x, b)
    | _ -> assert false (* invalid segment *)

  let add_point visited x = match Hashtbl.add visited x () with
    | `Duplicate -> raise (Found x)
    | `Ok -> ()

  let add_segments visited x1 x2 =
    range x1 x2 |> List.iter ~f:(add_point visited)
end

type direction = N | W | E | S
type move = direction * int
type step = L of int | R of int

let lex_steps lexbuf =
  let open Lexer in
  let rec go xs = function
    | EOF -> List.rev xs
    | LEFT n -> go (L n::xs) (read lexbuf)
    | RIGHT n -> go (R n::xs) (read lexbuf)
  in go [] (read lexbuf)

let parse s = Lexing.from_string s |> lex_steps

let move dir step = match dir, step with
  | N, L n | S, R n -> W, n
  | N, R n | S, L n -> E, n
  | W, L n | E, R n -> S, n
  | W, R n | E, L n -> N, n

let move_to_coord = function
  | N, n -> (0, n)
  | W, n -> (-n, 0)
  | E, n -> (n, 0)
  | S, n -> (0, -n)

let moves start_dir steps =
  let rec go (dir,_) moves = function
    | [] -> List.rev moves
    | step::steps -> let next = move dir step in go next (next::moves) steps in
  go (start_dir,0) [] steps

let () =
  let open Printf in
  let steps = In_channel.read_all "input.txt" |> parse in
  let end_pos = moves N steps |> List.map ~f:move_to_coord |> Coord.sum in
  printf "Part 1: end coordinate: (%d, %d), distance: %d\n"
    (fst end_pos) (snd end_pos) (Coord.dist (0,0) end_pos);
  let visited = Visited.create () in
  Visited.add_point visited (0,0);
  (* let steps = parse "R8, R4, R4, R8" in*)
  let end_pos =
    try
      List.(moves N steps
      |> map ~f:move_to_coord
      |> scan_left ~init:(0,0) ~f:Coord.(+)
      |> each_pair ~f:(Visited.add_segments visited)
      |> fun _ -> failwith "not found")
    with Visited.Found p -> p in
  printf "Part 2: end coordinate: (%d, %d), distance: %d\n"
    (fst end_pos) (snd end_pos) (Coord.dist (0,0) end_pos);
