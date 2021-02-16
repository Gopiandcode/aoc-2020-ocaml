open Containers

module CoordSet = Set.Make(struct type t = int * int [@@deriving ord] end)

let raw_input = IO.with_in "./day24/input" IO.read_all

let read_line str =
  let direction =
    let open Parse in
    let (-?>) s v = try_ (Parse.string s *> return v) in
    ("sw" -?> `SW) <|> ("se" -?> `SE) <|> ("nw" -?> `NW) <|>
    ("ne" -?> `NE)  <|> ("e" -?> `E) <|> ("w" -?> `W) in
  let line = Parse.many1 direction in
  Parse.parse_string_exn line str

let input txt = txt
                |> String.split_on_char '\n'
                |> List.filter (Fun.negate String.is_empty)
                |> List.map read_line

let (++) (a,b) (c,d) = (a + c, b + d)

let to_vec = function
  | `ID -> (0,0) | `NE -> (0,1) | `E -> (1,0) | `SE -> (1,-1)
  | `SW -> (0,-1) | `W -> (-1,0) | `NW -> (-1,+1)

let neighborhood point =
  [ `NE ; `E  ; `SE ; `SW; `W ; `NW ]
  |> List.to_iter |> Iter.map to_vec |> Iter.map ((++) point)

type colour = White | Black
let flip = function White -> Black | Black -> White
let is_black = function Black -> true | _ -> false

let lookup map coord = CoordSet.mem coord map |> function true -> Black | false -> White

let set map coord = function
  | White -> CoordSet.remove coord map
  | Black -> CoordSet.add coord map

let flip_coord map coord = lookup map coord |> flip |> set map coord

let no_black map coord =
  neighborhood coord |> Iter.map (lookup map) |> Iter.filter is_black |> Iter.length

let resolve_to_coord ls = List.to_iter ls |> Iter.map to_vec |> Iter.fold ((++)) (0,0)

let apply_flip tiling direction = flip_coord tiling (resolve_to_coord direction)

let count_black map = CoordSet.cardinal map

module Part1 = struct

  let result txt =
    input txt |> List.fold_left apply_flip CoordSet.empty |> count_black

end

module Part2 = struct

  let calculate_new_color map coord =
    let no_black = (no_black map coord) in
    function
    | Black when no_black = 0 || no_black > 2  -> White
    | White when  (no_black = 2) -> Black
    | v -> v

  let next_day tiling =
    CoordSet.to_iter tiling
    |> Iter.flat_map (fun v -> Iter.cons v (neighborhood v))
    |> Iter.uniq ~eq:(Pair.equal Int.equal Int.equal)
    |> Iter.fold (fun t c ->
        lookup tiling c |> calculate_new_color tiling c |> set t c 
      ) tiling

  let result txt =
    input txt
    |> List.fold_left apply_flip CoordSet.empty
    |> Fun.iterate 100 next_day
    |> count_black

end
