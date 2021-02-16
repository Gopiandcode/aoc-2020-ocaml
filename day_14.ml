open Containers

let raw_input = IO.with_in "./day14/input" IO.read_all
let fake_input = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"

let fake_input_2 = "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1"

type mask = Int64.t * Int64.t

let empty_bitstring = Int64.zero

let nth_bit n = (Int64.shift_left Int64.one n)

let set_bit n vl =
  Int64.(lor)
    (nth_bit n)
    vl

let get_bit n vl =
  Int64.(=) (Int64.(land) (Int64.shift_right_logical vl n) Int64.one)
    Int64.one

let read_mask t =
  let mask_txt = String.trim t in
  let active_map =
    mask_txt
    |> Iter.of_str
    |> Iter.rev
    |> Iter.foldi (fun str ind -> function
        | '0' | '1' -> str
        | _ -> set_bit ind str) empty_bitstring in
  let value_map =
    mask_txt
    |> Iter.of_str
    |> Iter.rev
    |> Iter.foldi (fun str ind -> function
        | '1' -> set_bit ind str | _ -> str) empty_bitstring in
  (active_map, value_map)

type operation =
    Operation of {ind: int; value: int;}
  | Mask of mask

let read_mem_access txt =
  match String.split_on_char '[' (String.trim txt) with
  | ["mem"; vl] -> Int.of_string_exn (String.sub vl 0 (String.length vl - 1))
  | ls -> failwith (Printf.sprintf  "invalid input [%s]" (List.to_string Fun.id ls))

let read_operation txt =
  match String.split_on_char '=' (String.trim txt) with
  | ["mask "; t] -> Mask (read_mask t)
  | [h;t] ->
    let ind = read_mem_access h in
    let value = Int.of_string_exn (String.trim t) in
    Operation {ind; value}
  | ls -> failwith (Printf.sprintf "invalid input [%s]" (List.to_string Fun.id ls))


let read_input txt =
  match String.split_on_char '\n' txt |> List.filter (Fun.negate String.is_empty) with
  | mask :: rest ->
    let mask = read_operation mask |> function Mask m -> m | _ -> failwith "invalid input" in
    let ops = List.map read_operation rest in
    mask, ops
  | ls -> failwith (Printf.sprintf "invalid input %s" (List.to_string Fun.id ls))

module Part1 = struct

  let apply_mask (d, b) c = let open Int64 in (lnot d land b) lor ( d land (Int64.of_int c))

  module IntMap = Map.Make (Int)

  let value_to_int map = IntMap.fold (Fun.const Int64.(+)) map Int64.zero |> Int64.to_int

  let run_operation (mask,state) =
    function
    | Operation {ind; value} ->
      (mask, IntMap.add ind (apply_mask mask value) state)
    | Mask mask ->
      (mask, state)

  let result input =
    let (mask, ops) = read_input input in
    List.fold_left run_operation (mask, IntMap.empty) ops
    |> snd
    |> value_to_int

  let () =
    result raw_input
    |> Fun.compose Int.to_string print_endline

end

module Part2 = struct

  type bit =
    | Value of bool
    | X [@@deriving eq, show]

  let to_bit = function
    | true, _ -> X
    | false, v -> Value v

  let mask_to_gen (active_bits, override_value) =
    let gen_bits n : bool Gen.t =
      Gen.int_range 0 (64-1)
      |> Gen.map (Fun.flip get_bit n) in
    Gen.zip (gen_bits active_bits) (gen_bits override_value)
    |> Gen.map to_bit
    |> Gen.to_list

  let write writing into =
    let rec loop prefix acc writing into = 
      match writing, into with
      | a :: writing, b :: into when equal_bit a b ->
        loop (a :: prefix) acc writing into
      | Value a :: _, Value b :: into when not Bool.(equal a b) ->
        let acc = (List.rev prefix @ Value b :: into) :: acc in
        acc
      | Value b :: writing, X :: into ->
        let acc = (List.rev prefix @ Value (not b) :: into) :: acc in
        loop (Value b :: prefix) acc writing into
      | X :: writing, Value a :: into -> loop (Value a :: prefix) acc writing into
      | _, _ -> List.rev acc in
    loop [] [] writing into

  let apply_mask (active_bits, override_value) value =
    (active_bits, Int64.(of_int value lor override_value))

  let add map value index =
    let rec loop acc map =
    match map with
      | [] -> List.rev ((value, index) :: acc)
      | (key, data) :: rest ->
        let acc =  
          List.map (Fun.flip Pair.make data) (write value key) @ acc in
        loop acc rest in
    loop [] map

  let count_xs = List.count (function X -> true | _ -> false)

  let contribution (index, value) =
    let no_xs = count_xs index in
    (Int.pow 2 no_xs) * value

  let total = List.fold_left (Fun.flip (Fun.compose contribution Int.(+))) 0

  let run_operation : mask * _ -> operation -> mask * _  =
    fun ((current_mask, map)) -> function
      | Operation { ind; value } ->
        let index = apply_mask current_mask ind |> mask_to_gen in
        let map = add map index value in
        (current_mask, map)
      | Mask mask ->  (mask, map)

  let result input =
    let (mask, ops) = read_input input in
    List.fold_left run_operation (mask, []) ops
    |> snd
    |> total

  let () = result raw_input |> Fun.compose Int.to_string print_endline

end
