open Core_kernel.Std
open Printf

let count_ones =
  let rec go ones x =
    if x > 0 then go (ones+1) (x land (x-1)) else ones in
  go 0

let is_open (x,y) =
  x*x + 3*x + 2*x*y + y + y*y + 1364 |> count_ones |> fun a -> a land 0b1 = 0

let neighs (x,y) = [x+1,y;x,y+1;x-1,y;x,y-1] |> List.filter ~f:is_open

let find_path target =
  let dist_tbl = Hashtbl.create ~hashable:Hashtbl.Poly.hashable () in
  Hashtbl.set dist_tbl ~key:(0,0) ~data:0;
  let add_neighs d coords  =
    List.iter coords (fun c -> Hashtbl.set dist_tbl ~key:c ~data:d) in
  let visited = Hashtbl.mem dist_tbl in
  let d coord = match Hashtbl.find dist_tbl coord with
    | Some n -> n
    | None -> 1 lsl 31 in
  let cmp_coords a b = d a - d b in
  let rec go heap =
    match Fheap.pop heap with
    | None -> assert false
    | Some (pos, heap) ->
      let dist = d pos in
      if pos = target then dist
      else begin
        let ns = neighs pos |> List.filter ~f:(fun c -> not (visited c)) in
        add_neighs (dist+1) ns;
        List.fold ns ~init:heap ~f:Fheap.add |> go
      end in
  go Fheap.(add (create ~cmp:cmp_coords) (0,0))

let () =
  let target = 31, 39 in
  find_path target |> printf "%d\n"
