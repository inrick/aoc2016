open Core_kernel.Std

module List = struct
  include List

  let scan_left xs ~init ~f =
    fold_left xs ~init:(init, []) ~f:(fun (x, zs) y ->
      let z = f x y in z, z::zs) |> snd |> rev
end

module Coord = struct
  type t = int * int

  let dist (a, b) (c, d) = abs (a - c) + abs (b - d)
  let (+) (a, b) (c, d) = (a + c, b + d)
  let ( * ) k (a, b) = (k*a, k*b)
  let show (a, b) = Printf.sprintf "(%d, %d)" a b
end

type step = L | R
type direction = N | E | S | W (* 0, 1, 2, 3 *)

let parse s =
  let ss = String.(strip s |> filter ~f:(Char.(<>) ',') |> split ~on:' ') in
  let steps = List.filter_map ss ~f:(fun s ->
    match s.[0] with
    | 'L' -> Some L
    | 'R' -> Some R
    | _ -> None) in
  let magnitudes =
    List.map ss ~f:(fun s -> String.slice s 1 0 |> int_of_string) in
  steps, magnitudes

let move prev dir = match prev, dir with
  | N, L | S, R -> W
  | E, L | W, R -> N
  | S, L | N, R -> E
  | W, L | E, R -> S

let move_to_coord = function (* x, y *)
  | N -> 0, 1
  | E -> 1, 0
  | S -> 0, -1
  | W -> -1, 0

let () =
  let open Printf in
  let steps, magns = In_channel.read_all "input.txt" |> parse in
  let end_coord = List.(scan_left steps ~init:N ~f:move
    >>| move_to_coord |> map2_exn magns ~f:Coord.( * )
    |> fold_left ~init:(0,0) ~f:Coord.(+)) in
  printf "Part 1: end coordinate: (%d, %d), distance: %d\n"
    (fst end_coord) (snd end_coord) (Coord.dist (0,0) end_coord);
