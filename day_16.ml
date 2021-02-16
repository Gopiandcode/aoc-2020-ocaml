open Containers

let raw_input = IO.with_in "./day16/input" IO.read_all

module StringMap = Map.Make(String)
module IntSet = Set.Make(Int)

type spec =
    Between of int * int
  | Or of spec * spec
[@@deriving show, eq]

(** determines whether a number lies in the spec range  *)
let rec number_valid_for_spec = function
  | Between (low, high) ->
    fun vl -> low <= vl && vl <= high
  | Or (specA, specB) ->
    let fA = number_valid_for_spec specA in
    let fB = number_valid_for_spec specB in
    fun vl -> fA vl || fB vl
  
type ticket_spec = spec StringMap.t
type ticket = int list

let fold_left1 f = function
  | [] -> invalid_arg "empty list"
  | h :: t -> List.fold_left f h t

let read_range txt =
  txt
  |> String.trim
  |> String.split_on_char '-'
  |> function
    [low; high] ->
    let of_string = Fun.compose String.trim Int.of_string_exn in
    Between (of_string low, of_string high)
  | _ -> failwith "invalid range"

let read_spec txt =
  txt
  |> String.split ~by:"or"
  |> List.map read_range
  |> fold_left1 (fun acc v -> Or (acc, v))

let read_field_spec txt =
  txt
  |> String.split_on_char ':'
  |> function
  | [field_name; spec] ->
    (field_name, read_spec spec)
  | _ -> failwith "invalid input"

let read_ticket_spec txt =
  txt
  |> String.split_on_char '\n' 
  |> List.filter (Fun.negate String.is_empty)
  |> List.map read_field_spec
  |> StringMap.of_list

let read_ticket txt : ticket =
  txt
  |> String.trim
  |> String.split_on_char ','
  |> List.map Int.of_string_exn

let drop_first txt =
  String.split_on_char '\n' txt
  |> List.filter (Fun.negate String.is_empty)
  |> List.tl

let repeat f txt =
  List.map f txt

let read_input txt =
  txt
  |> String.split ~by:"\n\n"
  |> function
  | [spec; mine; nearby] ->
    read_ticket_spec spec,
    drop_first mine |> List.hd |> read_ticket,
    drop_first nearby |> List.map read_ticket
  | _ -> failwith "invalid input"

let is_valid_number spec number =
  StringMap.values spec
  |> Iter.exists (Fun.flip number_valid_for_spec number)

module Part1 = struct

  let get_ticket_error_rate spec ticket =
    List.filter (Fun.negate (is_valid_number spec)) ticket
    |> List.fold_left Int.(+) 0

  let get_nearby_ticket_error_rate spec nearby =
    Iter.of_list nearby
    |> Iter.map (get_ticket_error_rate spec)
    |> Iter.fold Int.(+) 0

  let result input =
    let spec, _, nearby_tickets = read_input input in
    get_nearby_ticket_error_rate spec nearby_tickets

end

module Part2 = struct

  let is_valid_ticket spec ticket =
    List.for_all (is_valid_number spec) ticket

  let discard_invalid_tickets spec nearby_tickets =
    List.filter (is_valid_ticket spec) nearby_tickets

  type valid_range = IntSet.t * IntSet.t

  let compare_valid_range (incl_a, _) (incl_b, _) =
    Int.compare (IntSet.cardinal incl_a) (IntSet.cardinal incl_b)

  let add_index_to_valid_range ind (in_set, excl_set) =
    if not (IntSet.mem ind excl_set)
    then (IntSet.add ind in_set, excl_set)
    else (in_set, excl_set)

  let remove_index_from_valid_range ind (in_set, excl_set) =
    (IntSet.remove ind in_set, IntSet.add ind excl_set)

  let or_add f ind = function
    | None -> Some (f ind (IntSet.empty, IntSet.empty))
    | Some (in_set,excl_set) -> Some (f ind (in_set,excl_set))

  let update_valid_range ticket_spec mapping ind number =
    let update key value mapping =
      if number_valid_for_spec value number
      then StringMap.update key (or_add add_index_to_valid_range ind) mapping
      else StringMap.update key (or_add remove_index_from_valid_range ind) mapping
    in
    StringMap.fold update ticket_spec mapping

  let update_valid_range_from_ticket ticket_spec mapping ticket =
    List.foldi (update_valid_range ticket_spec) mapping ticket


  let add_mapping (added_fields, mapping) (name, (incl,_)) =
    let field = IntSet.diff incl added_fields |> IntSet.to_list |> List.hd in
    let added_fields = IntSet.union incl added_fields in
    let mapping = StringMap.add name field mapping in
    (added_fields, mapping)

  let determine_specification_names ticket_spec nearby_tickets =
    let range_map =
      List.fold_left
        (update_valid_range_from_ticket ticket_spec)
        StringMap.empty
        nearby_tickets in
    StringMap.to_iter range_map
    |> Iter.sort ~cmp:(Pair.compare (fun _ _ -> 0) compare_valid_range)
    |> Iter.fold add_mapping (IntSet.empty, StringMap.empty)
    |> snd

  let find_entries ticket =
    fun state (key, value) ->
    if String.prefix ~pre:"departure" key
    then state * List.nth ticket value
    else state

  let result input =
    let (spec, my_ticket, nearby_tickets) = read_input input in
    let valid_tickets = discard_invalid_tickets spec nearby_tickets in
    determine_specification_names spec valid_tickets
    |> StringMap.to_iter
    |> Iter.fold (find_entries my_ticket) 1

end
