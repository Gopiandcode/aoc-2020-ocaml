open Containers

let raw_input = IO.with_in "./day2/input" IO.read_all

type policy = MkPolicy of {min: int; max: int; char: char} [@@deriving show]

let read_range txt =
  match String.split_on_char '-' txt with
  | [h; t] ->
    Int.of_string_exn h, Int.of_string_exn t
  | _ -> invalid_arg "Bad line format."

let read_policy txt =
  match String.split_on_char ' ' txt with
  | [h; t] ->
    let (min,max) = read_range h in
    let char = String.get t 0 in
    MkPolicy {min; max; char}
  | _ -> invalid_arg "Bad line format."

let process_line txt =
  match String.split_on_char ':' txt with
  | [h; t] ->
    let policy = read_policy h in
    let txt = String.trim t in
    (policy, txt)
  | _ -> invalid_arg "Bad line format "

let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)
            |> List.map process_line

module Part1 = struct

  let count_occurances char str =
    Iter.of_str str
    |> Iter.fold (fun count char' ->
        if Char.(equal char char')
        then count + 1
        else count) 0

  let check_policy (MkPolicy {min=mi;max=ma;char}) str =
    let occurances = count_occurances char str in
    Int.((mi <= occurances) && (occurances <= ma))

  let result =
    List.filter (Fun.uncurry @@ check_policy) input
    |> List.length

end

module Part2 = struct

  let check_policy (MkPolicy {min=mi;max=ma;char}) str =
    let chr1 = String.get str (mi - 1) in
    let chr2 = String.get str (ma - 1) in
    let equal = Char.equal char in
    match equal chr1, equal chr2 with
    | true, false | false, true -> true
    | _ -> false

  let result = List.filter (Fun.uncurry @@ check_policy) input

  let result = List.length result

end
