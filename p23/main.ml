open Core_kernel.Std

type reg = A | B | C | D
type dest = Reg of reg | I of int
type instr =
  | Cpy of dest * reg
  | Jnz of dest * dest
  | Inc of reg
  | Dec of reg
  | Tgl of dest
  | Skip

let parse_instr s =
  let parse_reg = function
    | "a" -> A
    | "b" -> B
    | "c" -> C
    | "d" -> D
    | _ -> assert false in
  let parse_dest i = try Reg (parse_reg i) with _ -> I (int_of_string i) in
  let parts = String.split s ~on:' ' |> Array.of_list in
  match parts.(0) with
  | "cpy" -> Cpy (parse_dest parts.(1), parse_reg parts.(2))
  | "jnz" -> Jnz (parse_dest parts.(1), parse_dest parts.(2))
  | "inc" -> Inc (parse_reg parts.(1))
  | "dec" -> Dec (parse_reg parts.(1))
  | "tgl" -> Tgl (parse_dest parts.(1))
  | _ -> assert false

module Comp = struct
  type t = {a : int; b : int; c : int; d : int; pc : int}
    [@@deriving show]

  let initial = {a=7; b=0; c=0; d=0; pc=0}

  let inc_pc st = {st with pc=st.pc+1}

  let get st = function A -> st.a | B -> st.b | C -> st.c | D -> st.d

  let set st x = function
    | A -> {st with a=x}
    | B -> {st with b=x}
    | C -> {st with c=x}
    | D -> {st with d=x}

  let get_dest st = function
    | Reg x -> get st x
    | I i -> i

  let cpy st dest = set st (get_dest st dest)

  let jnz st cond i =
    let a = get_dest st i in
    if get_dest st cond = 0 then inc_pc st else {st with pc=st.pc+a}

  let tgl st x instrs =
    let pos = st.pc + get_dest st x in
    match instrs.(pos) with
    | Inc x -> instrs.(pos) <- Dec x
    | Dec x | Tgl (Reg x) -> instrs.(pos) <- Inc x
    | Jnz (x, Reg y) -> instrs.(pos) <- Cpy (x, y)
    | Cpy (x, y) -> instrs.(pos) <- Jnz (x, Reg y)
    | Tgl (I _) | Jnz (_, I _) -> instrs.(pos) <- Skip
    | Skip -> ()
    | exception _ -> ()

  let execute st instrs =
    let next st = function
      | Cpy (x, y) -> cpy st x y |> inc_pc
      | Jnz (x, y) -> jnz st x y
      | Inc x -> set st (get st x + 1) x |> inc_pc
      | Dec x -> set st (get st x - 1) x |> inc_pc
      | Tgl x -> tgl st x instrs; inc_pc st
      | Skip -> inc_pc st in
    let rec go st =
      match instrs.(st.pc) with
      | i -> go (next st i)
      | exception _ -> st in
    go st
end

let () =
  let instrs = In_channel.read_lines "input.txt"
    |> List.map ~f:parse_instr
    |> Array.of_list in
  Comp.(execute initial instrs |> show) |> print_endline;
