open Core_kernel.Std
open Printf

let solve elves =
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

let () =
  let elf = 1 + solve 3014387 in (* offset 0-based indexing *)
  printf "%d\n" elf;
