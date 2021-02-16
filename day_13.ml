open Containers

let raw_input = IO.with_in "./day13/input" IO.read_all
let fake_input = "939
7,13,x,x,59,x,31,19"

let read_id = function
  | "x" -> None
  | v -> Int.of_string v

let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)

module Part1 = struct 

  let data = input
             |> function
             | [earliest_timestamp; ids] ->
               let earliest_timestamp = Int.of_string_exn earliest_timestamp in
               print_endline ids;
               let ids = String.split_on_char ',' ids
                         |> List.filter_map read_id in
               (earliest_timestamp, ids)
             | _ -> failwith "invalid input"

  let to_wait_time earliest_time id =
    match earliest_time mod id with
    | 0 -> 0
    | v -> id - v

  let min f ls =
    let update state elt =
      if f elt < fst state
      then (f elt, elt)
      else state in
    match ls with
    | [] -> failwith "min of empty list"
    | h :: t -> snd @@ List.fold_left update (f h, h) t

  let find_earliest_id (earliest_time, ids) =
    ids |> min (to_wait_time earliest_time)

  let result =
    let id = find_earliest_id data in
    let wait_time = to_wait_time (fst data) id in
    id * wait_time

end

module Part2 () = struct
  open Z3

  module Constraints = struct
    let ctx = Z3.mk_context []
    let mk_int = Z3.Arithmetic.Integer.mk_numeral_i ctx
    let (+) a b = Z3.Arithmetic.mk_add ctx [a;b]
    let (mod) = Z3.Arithmetic.Integer.mk_mod ctx
    let (=) = Z3.Boolean.mk_eq ctx
    let (>) a b = Z3.Arithmetic.mk_gt ctx a b
    let all = Z3.Boolean.mk_and ctx
    let zero = (mk_int 0)
    let variable txt = Z3.Arithmetic.Integer.mk_const_s ctx txt
    let add_constraint t (offset,vl) = ((t + (mk_int offset)) mod (mk_int vl) = zero)
  end

  let data =
    List.nth input 1
    |> String.split_on_char ','
    |> Iter.of_list
    |> Iter.zip_i
    |> Iter.filter_map (fun (ind,vl) -> Int.of_string vl |> Option.map (Pair.make ind))
    |> Iter.to_list

  let solve ls =
    let solver = Z3.Solver.mk_solver Constraints.ctx None in
    let t = Constraints.variable "t" in
    let constraints = List.map (Constraints.add_constraint t) ls in
    match Z3.Solver.check solver (Constraints.(t > zero) :: constraints) with
    | Solver.UNSATISFIABLE -> failwith "unsatisfiable constraints"
    | Solver.UNKNOWN -> failwith "unknown constraints"
    | Solver.SATISFIABLE -> Z3.Solver.get_model solver |> Option.get_exn

  let model = solve data

  let result () = Z3.Model.get_decls model
                  |> function
                  | [fdecl] ->
                    Z3.Model.get_const_interp model fdecl |> Option.get_exn
                    |> Z3.Expr.to_string |> Int.of_string_exn
                  | _ -> failwith "could not find a model"
end
