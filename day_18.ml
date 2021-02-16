open Containers

let raw_input = IO.with_in "./day18/input" IO.read_all

module Equation = struct
  open CCParse
  type op = Add | Mul [@@deriving show, eq, ord]

  type t = | Number of int | Binop of op * t * t | Parens of t [@@deriving show, eq, ord]

  let read_number = chars1_if is_num >|= (fun v -> Number (Int.of_string_exn v))

  let read_parens read_op =
    let* result = (char '(') *>  read_op <* char ')' in
    pure (Parens result)

  let read_parens_or_number read_op = try_ (read_parens read_op) <|> try_ read_number

  let read_binop read_op char' f =
    let* num = read_parens_or_number read_op in
    let* () = skip_space in
    let* _ =  char char' in
    let* op = read_op in
    pure (f num op)

  let read_add read_op = read_binop read_op '+' (fun num rest -> Binop  (Add, num, rest))

  let read_mul read_op = read_binop read_op '*' (fun num rest -> Binop (Mul, num, rest))

  let read_op = fix (fun read_op ->
        skip_space *> begin
          try_ (read_add read_op)  <|> try_ (read_mul read_op)  <|> (read_parens_or_number read_op)
        end)

  let parse txt =
    let state = state_of_string txt in
    let result = ref None in
    read_op ~ok:(fun v -> result := Some v) ~err:raise state;
    Option.get_exn !result

end

let eval reduce (t: Equation.t) : int =
  let rec reduce_till_number t = match t with
    | Equation.Number n -> n
    | _ -> reduce_till_number (reduce t) in
  reduce_till_number t 

let parse_and_sum reduce input =
  String.split_on_char '\n' input
  |> Iter.of_list
  |> Iter.filter (Fun.negate String.is_empty)
  |> Iter.map Equation.parse
  |> Iter.map (eval reduce)
  |> Iter.fold Int.(+) 0

module Part1 = struct

    let rec reduce t = 
      match t with
      | Equation.Number n -> Equation.Number n
      | Equation.Binop (Equation.Add, Equation.Number a, Equation.Number b) -> Equation.Number (a + b)
      | Equation.Binop (Equation.Mul, Equation.Number a, Equation.Number b) -> Equation.Number (a * b)
      | Equation.Binop (Equation.Add, Equation.Number a, Equation.Binop (op, Equation.Number b, rest)) ->
        Equation.Binop (op, Equation.Number (a + b), rest)
      | Equation.Binop (Equation.Mul, Equation.Number a, Equation.Binop (op, Equation.Number b, rest)) ->
        Equation.Binop (op, Equation.Number (a * b), rest)
      | Equation.Binop (op, (Equation.Number _ as a), b) -> Equation.Binop (op, a, reduce b)
      | Equation.Binop (op, a, b) -> Equation.Binop (op, reduce a, b)
      | Equation.Parens ((Equation.Number _) as n) -> n
      | Equation.Parens v -> Equation.Parens (reduce v)

    let result input = parse_and_sum reduce input
end

module Part2 = struct

    let rec reduce t = 
      match t with
      | Equation.Number n -> Equation.Number n
      | Equation.Binop (Equation.Add, Equation.Number a, Equation.Number b) -> Equation.Number (a + b)
      | Equation.Binop (Equation.Mul, Equation.Number a, Equation.Number b) -> Equation.Number (a * b)
      | Equation.Binop (Equation.Add, Equation.Number a, Equation.Binop (op, Equation.Number b, rest)) ->
        Equation.Binop (op, Equation.Number (a + b), rest)
      | Equation.Binop (op, (Equation.Number _ as a), b) -> Equation.Binop (op, a, reduce b)
      | Equation.Binop (op, a, b) -> Equation.Binop (op, reduce a, b)
      | Equation.Parens ((Equation.Number _) as n) -> n
      | Equation.Parens v -> Equation.Parens (reduce v)

    let result input = parse_and_sum reduce input

end
