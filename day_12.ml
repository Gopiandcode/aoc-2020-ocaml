open Containers

let raw_input = IO.with_in "./day12/input" IO.read_all
let fake_input ="F10
N3
F7
R90
F11"

type direction =
  | North
  | East
  | South
  | West [@@deriving show]

type action =
  | Move of direction * int
  | TurnLeft of int
  | TurnRight of int
  | Forward of int [@@deriving show]

let read_action txt =
  let spec, count = String.take_drop 1 txt in
  let count = Int.of_string_exn count in
  match spec with
  | "N" -> Move (North, count)
  | "S" -> Move (South, count)
  | "E" -> Move (East, count)
  | "W" -> Move (West, count)
  | "L" -> TurnLeft (count / 90)
  | "R" -> TurnRight (count / 90)
  | "F" -> Forward count
  | _ -> failwith "invalid input"

let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)
            |> List.map read_action

module Part1 = struct
  type t = (direction * (int * int))
  let turn_left = function
    | North -> West
    | East -> North
    | South -> East
    | West -> South

  let turn_right = function
    | North -> East
    | East -> South
    | South -> West
    | West -> North

  let move_in_direction ~by (x,y) = function
    | North ->  (x, y - by)
    | South -> (x, y + by)
    | East -> (x + by, y)
    | West -> (x - by, y)

  let update (dir, pos) : action -> t = function
    | Forward count -> (dir, move_in_direction ~by:count pos  dir)
    | Move (move_dir, count) -> (dir, move_in_direction ~by:count pos move_dir)
    | TurnLeft count -> (Fun.iterate count turn_left dir, pos)
    | TurnRight count -> (Fun.iterate count turn_right dir, pos)

  let eval = List.fold_left update (East, (0,0))

  let manhatten_distance = function (_, (x,y)) -> abs x + abs y

  let result = eval input |> manhatten_distance
end


module Part2 = struct

  type t = ((int * int) * (int  * int))

  let move_in_direction ~by (x,y) = function
    | North ->  (x, y + by)
    | South -> (x, y - by)
    | East -> (x + by, y)
    | West -> (x - by, y)

  let move_towards_waypoint = function ((x,y), (wx,wy)) -> ((x + wx, y + wy), (wx,wy))

  let manhatten_distance = function ((x,y), _) -> abs x + abs y

  let turn_left (i,j) = (-j, i)

  let turn_right (i,j) = (j, -i)

  let update (pos, waypoint_pos) : action -> t = function
    | Move (move_dir, count) -> (pos, move_in_direction ~by:count waypoint_pos move_dir)
    | Forward count -> Fun.iterate count move_towards_waypoint (pos, waypoint_pos)
    | TurnLeft count -> (pos, Fun.iterate count turn_left  waypoint_pos)
    | TurnRight count -> (pos, Fun.iterate count turn_right waypoint_pos)

  let eval = List.fold_left update ((0,0), (10, 1))

  let result = eval input |> manhatten_distance

end
