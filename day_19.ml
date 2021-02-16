open Containers

module IntMap = Map.Make(Int)
let raw_input = IO.with_in "./day19/input" IO.read_all

type raw_rule = [
    `Char of char
  | `Index of int
  | `Or of raw_rule * raw_rule
  | `And of raw_rule * raw_rule
] [@@deriving show]

module RawRule = struct
  type t = raw_rule

  let rec matches_internal map str kont = function
    | `Char c ->
      if String.length str > 0 && Char.equal (String.get str 0) c
      then kont (String.drop 1 str)
      else None
    | `Index n -> matches_internal map str kont  (IntMap.find n map)
    | `Or (a,b) ->
        Option.or_lazy
          (matches_internal map str kont a)
          ~else_:(fun () -> matches_internal map str kont b )
    | `And (a,b) ->
      matches_internal map str (fun str -> matches_internal map str kont b) a

  let matches map str rule : bool =
    matches_internal map str (fun str -> Option.if_ String.is_empty str) rule
    |> Option.is_some

end

module Rule = struct
  type rule =
    | Char of char
    | Or of rule * rule
    | And of rule * rule [@@deriving show]

  let rec of_raw map : raw_rule -> rule = function
    | `Char c -> Char c
    | `Index ind -> of_raw map (IntMap.find ind map)
    | `Or (a,b) -> Or (of_raw map a, of_raw map b)
    | `And (a,b) -> And (of_raw map a, of_raw map b)      

  let rec matches_internal (ind, str) : rule -> _ = function
    | Char c ->
      if String.length str >= 1 && Char.equal (String.get str ind) c
      then Some (ind+1, str)
      else None
    | Or (a,b) ->
      Option.or_lazy
        (matches_internal (ind,str) a)
        ~else_:(fun () -> matches_internal (ind,str) b)
    | And (a,b) ->
      let open Option in
      let* (ind,str) = matches_internal (ind,str) a  in
      matches_internal (ind,str) b

  let matches str rule : bool =
    match matches_internal (0,str) rule with
    | None -> false
    | Some (ind, str) -> String.length str = ind
end

let fold1 f = Iter.fold (fun a b -> match a with None -> Some b | Some a -> Some (f a b)) None

let read_and_rule txt =
  txt
  |> String.trim
  |> String.split_on_char ' '
  |> Iter.of_list
  |> Iter.filter (Fun.negate String.is_empty)
  |> Iter.map String.trim
  |> Iter.map Int.of_string_exn
  |> Iter.map (fun v -> `Index v)
  |> fold1 (fun a b -> `And (a,b))
  |> Option.get_exn

let read_rule txt =
  if String.prefix ~pre:"\"" txt
  then `Char (String.get txt 1)
  else begin
    txt
    |> String.split_on_char '|'
    |> Iter.of_list
    |> Iter.filter (Fun.negate String.is_empty)
    |> Iter.map read_and_rule
    |> fold1 (fun a b -> `Or (a,b))
    |> Option.get_exn
  end

let read_spec_line txt =
  txt
  |> String.split_on_char ':'
  |> function
  | [h; t] -> Int.of_string_exn (String.trim h), read_rule (String.trim t)
  | _ -> failwith "invalid line"

let read_raw_rules txt : raw_rule IntMap.t =
  txt
  |> String.split_on_char '\n'
  |> List.to_iter
  |> Iter.filter (Fun.negate String.is_empty)
  |> Iter.map read_spec_line
  |> IntMap.of_iter

let build_rule_map txt =
  let map = read_raw_rules txt in
  let sanitize = Rule.of_raw map in
  IntMap.map sanitize map

let read_lines txt =
  txt
  |> String.trim
  |> String.split_on_char '\n'
  |> List.filter (Fun.negate String.is_empty)

module Part1 = struct

  let read_input txt =
    txt
    |> String.split ~by:"\n\n"
    |> function
    | [h;t] -> build_rule_map h, read_lines t
    | _ -> failwith "invalid input"

  let result txt =
    let (rule_map, str) = read_input txt in
    let main_rule = IntMap.find 0 rule_map in
    let is_valid txt = Rule.matches txt main_rule in
    List.count is_valid str

end

module Part2 = struct

  let read_input txt =
    txt
    |> String.split ~by:"\n\n"
    |> function
    | [h;t] -> read_raw_rules h, read_lines t
    | _ -> failwith "invalid input"

  let result txt =
    let (map, txts) = read_input txt in
    let map = map
              |> IntMap.add 8 (`Or (`Index 42, `And (`Index 42, `Index 8)))
              |> IntMap.add 11 (`Or (
                  `And (`Index 42, `Index 31),
                  `And (`Index 42, `And (`Index 11, `Index 31))
                )) in
    let main_rule = IntMap.find 0 map in
    let is_valid str = RawRule.matches map str main_rule in
    List.count is_valid txts

end
