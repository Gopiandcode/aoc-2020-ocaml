open Containers


let raw_input = IO.with_in "./day4/input" IO.read_all

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)


let expected_fields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

type passport = string StringMap.t

let read_pair txt =
  match String.split_on_char ':' txt with
  | [h;t] -> (String.trim h, String.trim t)
  | ls -> failwith (Format.sprintf "Invalid input. %a" (Format.pp_print_list Format.pp_print_string) ls)

let read_passport txt =
  String.split_on_char '\n' txt
  |> List.flat_map (String.split_on_char ' ')
  |> List.filter (Fun.negate String.is_empty)
  |> List.map String.trim
  |> List.map read_pair
  |> StringMap.of_list

let input =  raw_input |> String.split ~by:"\n\n" |> List.map read_passport

let pp_passport : passport Map.printer  = StringMap.pp String.pp String.pp
let show_passport vl = Format.sprintf "%a" pp_passport vl 

let (let+) x f = if x then f () else false
let (let@) x f = match x with None -> false | Some x -> f x

let validate_field field value =
  let is_n_digits n vl = Int.equal (String.length vl) n in
  let char_is_numeric = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false in
  let char_is_hexidecimal = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' -> true
    | _ -> false in
  match field with
  | "byr" ->
    let+ () = is_n_digits 4 value in
    let@ value = Int.of_string value in
    1920 <= value && value <= 2002
  | "iyr" ->
    let+ () = is_n_digits 4 value in
    let@ value = Int.of_string value in
    2010 <= value && value <= 2020
  | "eyr" -> 
    let+ () = is_n_digits 4 value in
    let@ value = Int.of_string value in
    2020 <= value && value <= 2030
  | "hgt" ->
    begin match String.suffix ~suf:"cm" value, String.suffix ~suf:"in" value with
      | true, false ->
        let (value, _) = String.take_drop (String.length value - 2) value in
        let@ value = Int.of_string value in
        150 <= value && value <= 193
      | false, true ->
        let (value, _) = String.take_drop (String.length value - 2) value in
        let@ value = Int.of_string value in
        59 <= value && value <= 76
      | _ -> false
    end
  | "hcl" ->
    let+ () = String.prefix ~pre:"#" value in
    Iter.of_str value
    |> Iter.drop 1
    |> Iter.for_all char_is_hexidecimal
  | "ecl" ->
    List.mem value ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
  | "pid" ->
    let+ () = is_n_digits 9 value in
    Iter.of_str value |> Iter.for_all char_is_numeric
  | "cid" -> true
  | _ -> false
    

let passport_is_valid passport =
  let present str = StringMap.mem str passport in
  List.for_all present expected_fields &&
  StringMap.to_list passport |> List.for_all (Fun.uncurry validate_field)


let result =
  input |> List.filter passport_is_valid |> List.length


let () = print_int result
