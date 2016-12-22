open Core_kernel.Std
open Printf
open Scanf

type node = {x : int; y : int; size : int; used : int; avail : int; use : int}

let parse s =
  sscanf s "/dev/grid/node-x%d-y%d %dT %dT %dT %d%%" @@
    fun x y size used avail use -> {x;y;size;used;avail;use}

let viable nodes =
  List.(cartesian_product nodes nodes
    |> count ~f:(fun (a, b) -> a.used <> 0 && a <> b && a.used <= b.avail))

let () =
  let nodes = In_channel.read_lines "input.txt"
    |> Fn.flip List.drop 2
    |> List.map ~f:parse in
  printf "%d\n" (viable nodes);
