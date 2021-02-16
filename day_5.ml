open Containers

let raw_input = IO.with_in "./day5/input" IO.read_all

type row_spec =  F | B [@@deriving show]
type column_spec =  R | L  [@@deriving show]

type range = int * int  [@@deriving show]

let upper_half (l,h) =
  let total = (h - l) + 1 in
  (l + total/2, h)

let lower_half (l,h) =
  let total = (h - l) + 1 in
  (l, h - total/2)

let extract (l,h) = if Int.equal l h then l else
    failwith (Format.sprintf "Range not completely determined %d %d"  l h)

let row_spec_from_char = function
  | 'F' -> F
  | 'B' -> B
  | _ -> failwith "Unknown character"

let col_spec_from_char = function
  | 'R' -> R
  | 'L' -> L
  | _ -> failwith "Unknown character"

let init_row_range = (0, 127)
let init_col_range = (0, 7)

let find_row_range txt =
  Iter.of_str txt
  |> Iter.map row_spec_from_char
  |> Iter.fold (Fun.flip (function
      | F -> lower_half
      | B -> upper_half
    )) init_row_range
  |> extract

let find_col_range txt =
  Iter.of_str txt
  |> Iter.map col_spec_from_char
  |> Iter.fold (Fun.flip (function
      | R -> upper_half
      | L -> lower_half
    )) init_col_range
  |> extract
  
  
let to_seat_id (row, col) = row * 8 + col

let process_string txt =
  let (row_spec, col_spec) = String.take_drop 7 txt in
  let row = find_row_range row_spec in
  let col = find_col_range col_spec in
  to_seat_id (row,col)


let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)
            |> List.map process_string

module Part1 = struct
  let result =
    List.fold_left (Int.max) (0) input

  let print_result () =
    print_int result
end

module Part2 = struct

  let ids = List.sort Int.compare input

  exception Success of int

  let missing_id =
    let min_id = List.fold_left Int.min Int.max_int ids in
    try
      ignore @@ List.fold_left (function
          | (Some prev, current) -> fun vl ->
            if Int.equal (prev + 1) current && Int.equal (current + 1) vl
            then (Some current, vl)
            else if Int.equal (prev + 1) current
            then raise (Success (current + 1))
            else raise (Success (prev + 1))
          | (None, current) -> fun vl -> (Some current, vl)
        ) (None, min_id) (List.tl ids);
      failwith "not found"
    with Success id -> id

  let print_result () = print_int missing_id

end
