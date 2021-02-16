open Containers

let raw_input = IO.with_in "./day6/input" IO.read_all

module CharSet = Set.Make (Char)

let to_char_set txt = txt
                      |> String.trim
                      |> Iter.of_str
                      |> Iter.fold (fun set char -> CharSet.add char set) CharSet.empty

let to_counts txt = txt
  |> String.split_on_char '\n' 
  |> List.map to_char_set
  |> (function
      | [] -> failwith "bad input"
      | h :: t -> List.fold_left CharSet.inter h t)
  |> CharSet.cardinal
  

let to_input txt =
  txt
  |> String.split ~by:"\n\n"
  |> List.filter (Fun.negate String.is_empty)
  |> List.map to_counts

let result txt = List.fold_left Int.(+) 0 (to_input txt)

let () = result raw_input |> print_int

