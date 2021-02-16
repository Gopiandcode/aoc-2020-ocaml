open Containers

let raw_input = Containers.IO.with_in "./day1/input" IO.read_all

let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)
            |> List.map Int.of_string_exn

module IntMap = Map.Make (Int)

let empty_map = IntMap.empty

module Part1 = struct

  let result =
    let exception Success of int * int in
    try
      ignore @@ List.fold_left
        (fun map num ->
           match IntMap.get num map with
           | Some old -> raise (Success (old, num))
           | None -> IntMap.add (2020 - num) num map
        ) empty_map input;
      failwith  "Could not find suitable pair."
    with
    | Success (a,b) -> (a,b)

  let result = fst result * snd result

end

module Part2 = struct


  type state = int IntMap.t * (int * int) IntMap.t


  exception Success of int * int * int

  let phase_1 ((a,_): state) = a
  let phase_2 ((_,b): state) = b

  let add_phase_1 ((a, b): state) ind value : state =
    (IntMap.add ind value a, b)
  let add_phase_2 ((a, b): state) ind value : state =
    (a, IntMap.add ind value b)

  let initial_state = (empty_map, empty_map) 

  let update (map: state) value =
    match IntMap.get value (phase_2 map) with
    | Some (fst,snd) -> raise (Success (fst,snd,value))
    | None ->
      let map = ref map in
      IntMap.iter
        (fun ind stored_value ->
           if ind > value then begin
             map := add_phase_2 !map (ind - value) (stored_value, value);
           end)
        (phase_1 !map);
      map := add_phase_1 !map (2020 - value) value;
      !map


  let result =
    try
      ignore @@ List.fold_left update initial_state input;
      failwith "could not find required pairing"
    with
    Success (a,b,c) -> (a,b,c)


  let result = let (a,b,c) = result in a * b  * c



end
