open Containers

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)
module IntMap = Map.Make (Int)

let raw_input = IO.with_in "./day21/input" IO.read_all

let is_word_char = (function '(' | ')' | ' ' | ',' -> false | _ -> true)

let allergens =
  let open Parse in
  char '(' *>
  string "contains" *>
  skip_white *>
  sep ~by:(string ", ") (chars_if is_word_char) <*
  char ')'

let ingredients =
  Parse.sep ~by:(Parse.char ' ') (Parse.chars_if is_word_char)
  |> Parse.map (List.filter (Fun.negate String.is_empty))

let recipe =
  let open Parse in
  let* ingredients = ingredients in
  let* _ = skip_white in
  let* allergens  = try_ allergens <|> return [] in
  return (ingredients, allergens)

let read_line = Parse.parse_string_exn recipe

let input txt =
  txt
  |> String.split_on_char '\n'
  |> List.filter (Fun.negate String.is_empty)
  |> List.map read_line

let get_allergens ls =
  let update st (_, allrgn) = StringSet.add_list st allrgn in
  List.fold_left update StringSet.empty ls |> StringSet.to_list

let build_ingredient_map ls =
  let count = ref 0 in
  let next_id () = let vl = !count in incr count; Some vl in
  let update map vl = StringMap.update vl (Option.or_lazy ~else_:next_id) map in
  List.fold_left (fun map (ingr, _) -> List.fold_left update map ingr) StringMap.empty ls
  
module Model = struct
  let ctx = Z3.mk_context []
  let int = Z3.Arithmetic.Integer.mk_sort ctx
  let set = Z3.Set.mk_sort ctx int
  let empty_set = Z3.Set.mk_empty ctx int
  let set_of vls = List.fold_left (Z3.Set.mk_set_add ctx) empty_set vls
  let to_int n = n |> Z3.Expr.to_string |> Int.of_string_exn
  let mem x set = Z3.Set.mk_membership ctx x set
  let value v = Z3.Arithmetic.Integer.mk_numeral_i ctx v
  let variable str = Z3.Expr.mk_const_s ctx str int
  let _and = Z3.Boolean.mk_and ctx
  let _or = Z3.Boolean.mk_or ctx
  let _add = Z3.Arithmetic.mk_add ctx
  let _sub = Z3.Arithmetic.mk_sub ctx
  let (&&&) a b =  _and [a;b]
  let (|||) a b =  _or [a;b]
  let (++) a b =  _add [a;b]
  let (--) a b =  _sub [a;b]
  let (==) =  Z3.Boolean.mk_eq ctx
  let (!!) = Z3.Boolean.mk_not ctx
  let (!==) a b =  !! (a == b)

  let is_unique ls = 
    let rec loop acc = function
      | [] -> acc
      | h :: t ->
        loop (List.map ((!==) h) t @ acc) t in
    loop [] ls |> _and

  type t = {
    ingredients: Z3.Expr.expr StringMap.t;
    allergens: Z3.Expr.expr StringMap.t;
    constraints: Z3.Expr.expr;
    solver: Z3.Solver.solver;
  }

  let (.@[]) t = function
    | `Ingredient vl -> StringMap.find vl t.ingredients
    | `Allergen vl ->  StringMap.find vl t.allergens

  let get_model t =
    ignore @@ Z3.Solver.check t.solver [];
    let model = Z3.Solver.get_model t.solver|> Option.get_exn in
    model

  let allergen_map t =
    let model = get_model t in
    let eval expr =
      Z3.Model.eval model expr true
      |> Option.get_exn
      |> to_int in
    t.allergens |> StringMap.map eval

  let get_cannonical_list t =
    let map = allergen_map t in
    let ingr_map = t.ingredients
                   |> StringMap.to_iter
                   |> Iter.map Pair.swap
                   |> Iter.map (Pair.map_fst to_int)
                   |> IntMap.of_iter in
    StringMap.to_iter map
    |> Iter.map (fun (allergen, id) -> (allergen, IntMap.find id ingr_map))
    |> Iter.sort ~cmp:(fun (a,_) (b,_) -> String.compare a b)
    |> Iter.map snd
    |> Iter.to_list
    |> String.concat ","


  let build_constraint (ingrs, alrgns) =
    let ingrs = set_of ingrs in
    List.map (fun alrgn -> mem alrgn ingrs) alrgns

  let convert_data ingredients allergens =
    List.map (fun (ingr,alrgns) -> (List.map (Fun.flip StringMap.find ingredients) ingr,
                                    List.map (Fun.flip StringMap.find allergens) alrgns))     

  let build data =
    let ingredients =
      build_ingredient_map data
      |> StringMap.map (fun n -> value n) in
    let allergens =
      get_allergens data
      |> List.map (fun str -> str, variable str)
      |> StringMap.of_list in
    let data = convert_data ingredients allergens data in
    let allergens_are_unique = StringMap.values allergens |> Iter.to_list |> is_unique in
    let allergens_occur_in_ingredients = List.flat_map build_constraint data in
    let constraints = _and @@ allergens_are_unique :: allergens_occur_in_ingredients in
    let solver = Z3.Solver.mk_solver ctx None in
    Z3.Solver.add solver [constraints];
    {ingredients; allergens; constraints; solver}

  let is_possible_allergen t ingredient =
    let query = 
      let ingredient = t.@[`Ingredient ingredient] in
      let allergens = StringMap.values t.allergens |> Iter.to_list |> set_of in
      mem ingredient allergens in
    match Z3.Solver.check t.solver [query] with
    | Z3.Solver.UNSATISFIABLE -> false
    | Z3.Solver.UNKNOWN -> invalid_arg "unknown"
    | Z3.Solver.SATISFIABLE -> true

end

module Part1 = struct

  let count_occurrences vls data =
    let set = StringSet.of_list vls in
    let update count (ingrs, _) = count + List.count (Fun.flip StringSet.mem set) ingrs in
    List.fold_left update 0 data

  let result txt =
    let data = input txt in
    let model = Model.build data in
    let ingredients = build_ingredient_map data |> StringMap.keys |> Iter.to_list in
    let free_ingredients = List.filter (Fun.negate (Model.is_possible_allergen model)) ingredients in
    count_occurrences free_ingredients data

  let print_result () = result raw_input |> Fun.compose Int.to_string print_endline

end

module Part2 = struct

  let result txt =
    let data = input txt in
    let model = Model.build data in
    Model.get_cannonical_list model

  let () = result raw_input |> print_endline

end
