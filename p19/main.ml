open Core_kernel.Std
open Printf

let solve elves =
  let ps = Array.create ~len:elves true in
  let rec fix n =
    let rec steal m =
      let m = m mod elves in
      if ps.(m) then m else steal (m+1) in
    let n = n mod elves in
    if not ps.(n) then fix (n+1)
    else
      let m = steal (n+1) in
      if m = n then n else (ps.(m) <- false; fix (n+1)) in
  fix 0

let () =
  let elf = 1 + solve 3014387 in (* offset 0-based indexing *)
  printf "%d\n" elf;
