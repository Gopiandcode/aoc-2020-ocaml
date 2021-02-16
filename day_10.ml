open Containers

let raw_input = IO.with_in "./day10/input" IO.read_all

let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)
            |> List.map Int.of_string_exn

module Part1 = struct

  let find_jolt_distribution =
    input
    |> List.sort Int.compare
    |> Iter.of_list
    |> Iter.fold (fun (prev_vl, no_1, no_2, no_3) vl ->
        print_endline @@ Printf.sprintf "got %d and %d" prev_vl vl;
        match vl - prev_vl with
        | 0 -> (vl, no_1, no_2, no_3)
        | 1 -> (vl, no_1 + 1, no_2, no_3)
        | 2 -> (vl, no_1, no_2 + 1, no_3)
        | 3 -> (vl, no_1, no_2, no_3 + 1)
        | n -> failwith (Printf.sprintf "invalid difference %d" n)) (0,0,0,0)
    |> (fun (_, no_1, no_2, no_3) -> (no_1, no_2, no_3 + 1))

  let result =
    let (no_1, _, no_3) = find_jolt_distribution in
    no_1 * no_3

end

module Part2 = struct

  module IntMap = Map.Make (Int)

  let update state n =
    let get_count v = IntMap.find_opt v state  |> Option.get_or ~default:0 in
    let count = get_count (n - 1) + get_count (n - 2) + get_count (n - 3) in
    IntMap.add n count state

  let find_possible_counts ls =
    let device_count =
      match ls with [] -> failwith "empty adapters" | h :: t -> List.fold_left Int.max h t + 3 in
    let count_map =
      let init = IntMap.empty |> IntMap.add 0 1 in
      Iter.of_list ls
      |> Iter.sort ~cmp:Int.compare
      |> Iter.fold update init  in
    IntMap.find  device_count (update count_map device_count)

end
