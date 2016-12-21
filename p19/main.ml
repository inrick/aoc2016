open Core_kernel.Std
open Printf

let solve1_brute elves =
  let ps = Array.create ~len:elves true in
  let rec steal i =
    let i = i mod elves in
    if ps.(i) then i else steal (i+1) in
  let rec fix i =
    let i = i mod elves in
    if not ps.(i) then fix (i+1)
    else
      let j = steal (i+1) in
      if j = i then i else (ps.(j) <- false; fix (i+1)) in
  fix 0

(* worked out from brute force solution *)
let solve1 elves =
  let rec go n winner =
    if n = elves then winner
    else if n = winner then go (n+1) 1
    else go (n+1) (winner+2) in
  go 1 1

(* used for inspiration to work out the calculation *)
let solve2_brute elves =
  let ps = Array.create ~len:elves true in
  let skip n from =
    let rec go m i =
      let i = i mod elves in
      if ps.(i) then
        if m >= n then i else go (m+1) (i+1)
      else go m (i+1) in
    go 1 (from+1) in
  let rec fix i left =
    let rec steal m =
      let m = m mod elves in
      if ps.(m) then m else steal (m+1) in
    let i = i mod elves in
    if not ps.(i) then fix (i+1) left
    else
      let m = steal (skip (left/2) i) in
      if m = i then i else (ps.(m) <- false; fix (i+1) (left-1))
    in
  fix 0 elves

(* worked out with output from above function *)
let solve2 elves =
  let rec go n winner last =
    if n = elves then winner
    else if n = winner then go (n+1) 1 winner
    else if winner < last then go (n+1) (winner+1) last
    else go (n+1) (winner+2) last in
  go 1 1 1

let () =
  let input = 3014387 in
  let elf = solve1 input in (* offset 0-based indexing *)
  printf "%d\n" elf;
  let elf2 = solve2 input in
  printf "%d\n" elf2;
