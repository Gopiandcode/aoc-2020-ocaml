open Containers

let raw_input = IO.with_in "./day25/input" IO.read_all

let input txt =
  txt
  |> String.split_on_char '\n'
  |> List.filter_map Int.of_string
  |> List.hd_tl
  |> Pair.map_snd List.hd

let div_num = 20201227

let transform subject_number loop_size =
  let update vl = (vl * subject_number) mod div_num in
  Fun.iterate loop_size update 1

let transform_leaky sn ls = (Int.pow sn ls) mod div_num

let public_key loop_size = transform 7 loop_size

let invert_transform result sn =
  let rec loop ind prod =
    if prod = result
    then ind
    else loop (ind + 1) ((prod * sn) mod div_num) in
  loop 0 1

let handshake card_sls door_sls =
  let card_pub_key = public_key card_sls in
  let door_pub_key = public_key door_sls in
  let encryption_key = transform card_pub_key door_sls in
  let encryption_key' = transform door_pub_key card_sls in
  (card_pub_key, door_pub_key, encryption_key, encryption_key')

let find_encryption_key (card_pub_key, door_pub_key) =
  let sls = invert_transform card_pub_key 7  in
  transform door_pub_key sls

let () =
  input raw_input
  |> find_encryption_key
  |> Fun.compose Int.to_string print_endline
