open Core_kernel.Std

module M = Map.Make(Int)

type botid = int
type dest = Output of int | Bot of botid
type instr =
  | Bot_yield of botid * dest * dest
  | Input of int * botid

type bot = {low : int option; high : int option; low_to : dest; high_to : dest}

let give_bot x bot = match bot.low, bot.high with
  | None, None -> {bot with low=Some x}
  | Some l, None when l < x -> {bot with high=Some x}
  | Some _, None -> {bot with low=Some x; high=bot.low}
  | None, Some h when x < h -> {bot with low=Some x}
  | None, Some _ -> {bot with low=bot.high; high=Some x}
  | Some _, Some _ -> assert false

let parse s =
  let tokens = String.split s ~on:' ' |> Array.of_list in
  let int = int_of_string in
  let parse_dest n = match tokens.(n) with
    | "bot" -> Bot (int tokens.(n+1))
    | "output" -> Output (int tokens.(n+1))
    | _ -> assert false in
  match tokens.(0) with
  | "bot" -> Bot_yield (int tokens.(1), parse_dest 5, parse_dest 10)
  | "value" -> Input (int tokens.(1), int tokens.(5))
  | _ -> assert false

let process target instrs =
  let add_bot bots bid bot = M.add bots ~key:bid ~data:bot in
  let find_bot bots = M.find_exn bots in
  let bots = List.fold instrs ~init:M.empty ~f:(fun bots -> function
    | Bot_yield (bid, low_to, high_to) ->
      add_bot bots bid {low=None; high=None; low_to; high_to}
    | Input _ -> bots) in
  let inputs = List.filter_map instrs ~f:(function
    | Input (x, bid) -> Some (x, bid)
    | _ -> None) in
  let rec go inputs bots k = match inputs with
    | [] -> k bots
    | (x, bid)::is -> send_to bots x (Bot bid) (fun bots -> go is bots k)
  and send_to bots x dest k = match dest with
    | Bot bid ->
      let bot = give_bot x (find_bot bots bid) in
      let bots = add_bot bots bid bot in
      (match bot.low, bot.high with
      | Some l, Some h ->
        if (l, h) = target then printf "Bot id: %d\n" bid;
        let bots = add_bot bots bid {bot with low=None} in
        send_to bots l bot.low_to (fun bots ->
          let bots = add_bot bots bid {bot with high=None} in
          send_to bots h bot.high_to k)
      | _, _ -> k bots)
    | Output i -> printf "Output %d got value %d\n" i x; k bots in
  go inputs bots (fun bots -> ())

let () =
  let instrs = In_channel.read_lines "input.txt" |> List.map ~f:parse in
  process (17, 61) instrs;
