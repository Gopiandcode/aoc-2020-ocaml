open Containers

let raw_input = IO.with_in "./day15/input" IO.read_all

let input = raw_input
            |> String.split_on_char ','
            |> List.filter_map Int.of_string

module IntMap = Map.Make (Int)

let (.@[]) map vl = IntMap.get vl map
let (.@[]<-) map key data = IntMap.add key data map

module Part1 = struct

  let update prev_turn (map,prev_number) =
    match IntMap.find_opt prev_number map with
    | None ->
      ((map.@[prev_number] <- prev_turn), 0)
    | Some last_seen_turn ->
      let map = (map.@[prev_number] <- prev_turn) in
      (map, prev_turn - last_seen_turn)

  let gen_state data =
    let first, last = List.take_drop (List.length data - 1) data in
    let last = List.hd last in
    let map = List.mapi (fun ind vl -> (vl, ind + 1)) first |> IntMap.of_list in
    let prev_turn = List.length data in
    prev_turn, (map, last)

  let run_until_turn target =
    let rec loop prev_turn state =
      let current_state = update prev_turn state in
      let current_turn = prev_turn + 1 in
      if current_turn < target
      then loop current_turn current_state
      else current_state in
    loop

  let run target data =
    let initial_prev_turn, initial_state = gen_state data in
    snd @@ run_until_turn target initial_prev_turn initial_state

  let result = run 2020 input

  let print_result = Fun.compose Int.to_string print_endline result

end 


module Part2 = struct

  let result = Part1.run 30000000 input (* n00b but it works *)

  let print_result = Fun.compose Int.to_string print_endline result

end 
