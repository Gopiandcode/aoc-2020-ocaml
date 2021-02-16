open Containers

module IntSet = Set.Make (Int)
type t = (int * IntSet.t) list

let raw_input = IO.with_in "./day9/input" IO.read_all
let fake_input = "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576"

let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)
            |> List.map Int.of_string_exn

module Part1 = struct
  let build_pairs preamble : t =
    let rec loop acc = function
      | [] -> List.rev acc
      | h :: t ->
        loop ((h, IntSet.of_list @@ List.map (Int.(+) h) t) :: acc) t in
    loop [] preamble

  let check_is_sum vl (t: t) = List.exists (Fun.compose snd (IntSet.mem vl)) t

  let add_new_value vl (t: t) : t =
    let t =  List.tl t in
    let rec loop acc = function
      | [] -> List.rev ((vl, IntSet.empty) :: acc)
      | (h, hS) :: t ->
        loop ((h, IntSet.add (h + vl) hS) :: acc) t in
    loop [] t

  let find_non_sum ~n ls =
    let preamble, remain = List.take_drop n ls in
    let state = build_pairs preamble in
    let exception Success of int in
    let check_input state vl =
      if check_is_sum vl state
      then add_new_value vl state
      else raise (Success vl) in
    try
      ignore @@ List.fold_left check_input state remain;
      failwith "non-sum not found"
    with
      Success v -> v

  let result =   find_non_sum ~n:25 input

  let print_result () =
    result
    |> print_int
end


module Part2 = struct

  exception Success of int * int

  let update goal prev ind vl =
    (ind, goal - vl) :: List.filter_map (fun (start_index, remainder) ->
        match remainder - vl with
        | 0 -> raise (Success (start_index, ind))
        | v when Int.(v >= 0) -> Some (start_index, v)
        | _ -> None) prev 

  let find_contiguous_sum_to goal ls =
    try
      ignore @@ List.foldi (update goal) [] ls;
      failwith "not found"
    with Success (s,v) -> (s,v)


  let find_result ls (st,ed) =
    let sub_list = List.drop st ls |> List.take (ed - st + 1) in
    let min = function
        [] -> failwith "minimum of empty list"
      | h :: t -> List.fold_right (Int.min) t h in
    let max = function
        [] -> failwith "maximum of empty list"
      | h :: t -> List.fold_right (Int.max) t h in
    min sub_list + max sub_list
            

  let result =
    find_contiguous_sum_to Part1.result input
    |> find_result input


end
