open Containers

let raw_input = IO.with_in "./day13/input" IO.read_all

let read_id = function
  | "x" -> None
  | v -> Int.of_string v

let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)

module Part1 = struct 

  let data = input
             |> function
             | [earliest_timestamp; ids] ->
               let earliest_timestamp = Int.of_string_exn earliest_timestamp in
               print_endline ids;
               let ids = String.split_on_char ',' ids
                         |> List.filter_map read_id in
               (earliest_timestamp, ids)
             | _ -> failwith "invalid input"

  let to_wait_time earliest_time id =
    match earliest_time mod id with
    | 0 -> 0
    | v -> id - v

  let min f ls =
    let update state elt =
      if f elt < fst state
      then (f elt, elt)
      else state in
    match ls with
    | [] -> failwith "min of empty list"
    | h :: t -> snd @@ List.fold_left update (f h, h) t

  let find_earliest_id (earliest_time, ids) =
    ids |> min (to_wait_time earliest_time)

  let result =
    let id = find_earliest_id data in
    let wait_time = to_wait_time (fst data) id in
    id * wait_time

end

module Part2 = struct
  let to_mod_vl (offset, base) =
    (- offset, base)

  let data =
    List.nth input 1
    |> String.split_on_char ','
    |> Iter.of_list
    |> Iter.zip_i
    |> Iter.filter_map
      (fun (ind,vl) ->
         Int.of_string vl |> Option.map (Pair.make ind))
    |> Iter.map to_mod_vl
    |> Iter.to_list


  let ( * ) a b = Z.mul a b
  let ( + ) a b = Z.add a b
  let ( - ) a b = Z.sub a b
  let ( / ) a b = Z.div a b
  let (mod) a b = Z.(mod) a b
  let one = Z.one
  let zero = Z.zero

  let gcd_ext a b =
    let rec loop old_r r old_s s old_t t =
      if Z.(equal r zero)
      then (old_s, old_t)
      else
        let quotient =  old_r / r in
        loop
          r (old_r - (quotient * r))
          s (old_s - (quotient * s))
          t (old_t - (quotient * t)) in
    loop a b one zero zero one

  let update (r1, m1) (r2, m2) =
    let r2,m2 = Z.of_int r2, Z.of_int m2 in
    let (s, t) = gcd_ext m1 m2 in
    let x = (r1 * t * m2) + (r2 * s * m1)  in
    (x, m1 * m2)

  let result it =
    let h,t = List.hd_tl it in
    let h = Pair.map Z.of_int Z.of_int h in
    let x,y = List.fold_left update h t in
    y + (y + x) mod y

  let () = result data |> Z.to_string |> print_endline

end
