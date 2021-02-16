[@@@warning "-26"]
open Containers

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let fake_input = "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."

let empty_bitstring = Int.zero
let nth_bit n = (Int.shift_left Int.one n)
let get_bit n vl = Int.(land) (nth_bit n) vl > 0
let set_bit n vl = Int.(lor) (nth_bit n) vl
let count_bits n str =
  Iter.int_range ~start:0 ~stop:(n - 1)
  |> Iter.fold (fun count ind -> if get_bit ind str then count + 1 else count) 0 
let drop_last str = String.take (String.length str - 1) str

module Image = struct

  type row = Int.t

  type t = { size: int; top: int; right: int; bottom: int; left: int; } [@@deriving show, eq]

  let invert size row =
    let update str ind =
      if get_bit (size - ind - 1) row then set_bit ind str else str in
    Iter.int_range ~start:0 ~stop:(size - 1)
    |> Iter.fold update empty_bitstring

  let edges t =
    let inv = invert t.size in
    [ t.top; t.right; t.bottom; t.left; inv t.top; inv t.right; inv t.bottom; inv t.left; ]

  let rotate_left img =
    { img with
      top=img.right; right=invert img.size img.bottom;
      bottom=img.left; left=invert img.size img.top }

  let rotate_left2 img =
    { img with
      top= invert img.size img.bottom; right=invert img.size img.left;
      bottom=invert img.size img.top; left=invert img.size img.right; }

  let rotate_left3 img =
    { img with
      top= invert img.size img.left; right=img.top;
      bottom=invert img.size img.right; left=img.bottom; }

  let flip_h img =
    {img with top=img.bottom; bottom=img.top;
              left=invert img.size img.left;
              right=invert img.size img.right; }

  let permutations = [
    Fun.id; rotate_left; rotate_left2; rotate_left3;
    flip_h; Fun.compose rotate_left flip_h;
    Fun.compose rotate_left2 flip_h; Fun.compose rotate_left3  flip_h; ]

  let matches_horiz l r = l.right = r.left
  let matches_vert t b = t.bottom = b.top

  let read_row _len txt =
    List.foldi (fun str ind -> function
          '.' -> str
        | '#' -> set_bit ind str
        | c -> failwith (Printf.sprintf "unknown char %c" c)) empty_bitstring txt

  let read body =
    let last ls = List.last_opt ls |> Option.get_exn in
    let size = String.length (List.hd body) in
    let top = List.nth body 0 |> String.to_list |> read_row size in
    let bottom = body |> last |> String.to_list |> read_row size in
    let (left, right) =
      let sanitize = Fun.compose List.rev (read_row size) in
      let update (l,r) v = (String.get v 0 :: l, String.get v (String.length v - 1) :: r) in
      List.fold_left update ([],[]) body |> Pair.map sanitize sanitize in
    {size; top;right;bottom;left;}

end

module Solver (Input: sig val data: (int * Image.t) list end) = struct
  let ctx = Z3.mk_context []
  let int = Z3.Arithmetic.Integer.mk_sort ctx
  let variable str = Z3.Expr.mk_const_s ctx str int
  let const n = Z3.Arithmetic.Integer.mk_numeral_i ctx n
  let (==) = Z3.Boolean.mk_eq ctx
  let (<>) a b = Z3.Boolean.mk_not ctx (a == b)
  let and_ a = Z3.Boolean.mk_and ctx a
  let or_ a = Z3.Boolean.mk_or ctx a
  let (&&&) a b = and_ [a;b]
  let (<<) a b = Z3.Arithmetic.mk_lt ctx a b
  let (<<=) a b = Z3.Arithmetic.mk_le ctx a b
  let (!!) a = Z3.Boolean.mk_not ctx a
  let (|~>) a b =   Z3.Boolean.mk_implies ctx a b
  let rec unique = function [] -> [] | h :: t ->  (List.map ((<>) h) t ) @ unique t

  let image_dim = List.length Input.data |> Float.of_int |> sqrt |> Int.of_float

  let (.![]) map ind =
    let get_ind row col = Array.get map (image_dim * row + col) in
    let top (t,_,_,_) = t in
    let right (_,r,_,_) = r in
    let bottom (_,_,b,_) = b in
    let left (_,_,_,l) = l in
    match ind with
    | `Left (row,col) -> left @@ get_ind row col
    | `Right (row,col) -> right @@ get_ind row col
    | `Top (row,col) -> top @@ get_ind row col
    | `Bottom (row,col) -> bottom @@ get_ind row col
  let (.@[]) map id =
    let get_id id =
      IntMap.find id map in
    let top (t,_,_,_,_) = t in
    let right (_,r,_,_,_) = r in
    let bottom (_,_,b,_,_) = b in
    let left (_,_,_,l,_) = l in
    let assign (_,_,_,_,a) = a in
    match id with
    | `Left id -> left @@ get_id id
    | `Right id -> right @@ get_id id
    | `Top id -> top @@ get_id id
    | `Bottom id -> bottom @@ get_id id
    | `Assign id -> assign @@ get_id id

  let image_variables =
    Iter.product
      (Iter.int_range ~start:0 ~stop:(image_dim - 1))
      (Iter.int_range ~start:0 ~stop:(image_dim - 1))
    |> Iter.map (fun (row,col) ->
        variable (Printf.sprintf "grid_(%dx%d)_top" row col),
        variable (Printf.sprintf "grid_(%dx%d)_right" row col),
        variable (Printf.sprintf "grid_(%dx%d)_bottom" row col),
        variable (Printf.sprintf "grid_(%dx%d)_left" row col))
    |> Iter.to_array

  let tile_variables =
    Input.data
    |> Iter.of_list
    |> Iter.map fst
    |> Iter.map (fun id ->
        id, (variable @@ Printf.sprintf "tile_%d_top" id,
             variable @@ Printf.sprintf "tile_%d_right" id,
             variable @@ Printf.sprintf "tile_%d_bottom" id,
             variable @@ Printf.sprintf "tile_%d_left" id,
             variable @@ Printf.sprintf "tile_%d_assign" id))
    |> IntMap.of_iter

  let base_image_constraints =
    Iter.product
      (Iter.int_range ~start:0 ~stop:(image_dim - 1))
      (Iter.int_range ~start:0 ~stop:(image_dim - 1))
    |> Iter.flat_map_l (fun (row,col) ->
        let if_ b v = if b then [v ()] else [] in
        let top_constraints =
          if row > 0
          then [image_variables.![`Bottom (row-1,col)] == image_variables.![`Top (row,col)]]
          else [] in
        let left_constraints =
          if col > 0
          then [image_variables.![`Right (row,col-1)] == image_variables.![`Left (row,col)]]
          else [] in
        let right_constraints =
          if col < image_dim - 1
          then [image_variables.![`Left (row,col+1)] == image_variables.![`Right (row,col)]]
          else [] in
        let bottom_constraints =
          if row < image_dim - 1
          then [image_variables.![`Top (row+1,col)] == image_variables.![`Bottom (row,col)]]
          else [] in
        top_constraints @ left_constraints @ right_constraints @ bottom_constraints
      ) |> Iter.to_list

  let tile_eq_constraint id (tile: Image.t) =
    and_
    [(tile_variables.@[`Left id] == const tile.Image.left);
    (tile_variables.@[`Right id] == const tile.Image.right);
    (tile_variables.@[`Top id] == const tile.Image.top);
    (tile_variables.@[`Bottom id] == const tile.Image.bottom)]

  let tile_assign_constraint id =
    let assign_variable = tile_variables.@[`Assign id] in
    let left_variable = tile_variables.@[`Left id] in
    let right_variable = tile_variables.@[`Right id] in
    let top_variable = tile_variables.@[`Top id] in
    let bottom_variable = tile_variables.@[`Bottom id] in
    (const 0 <<= assign_variable) &&&
    (assign_variable << const (image_dim * image_dim)) &&&
    (Iter.product
       (Iter.int_range ~start:0 ~stop:(image_dim - 1))
       (Iter.int_range ~start:0 ~stop:(image_dim - 1))
     |> Iter.map (fun (row,col) ->
         let ind = row * image_dim + col in
         (assign_variable == const ind) |~>
         and_
           [(image_variables.![`Left (row,col)] == left_variable);
            (image_variables.![`Right (row,col)] == right_variable);
            (image_variables.![`Bottom (row,col)] == bottom_variable);
            (image_variables.![`Top (row,col)] == top_variable)]
       )
     |> Iter.to_list
     |> and_)

  let tile_value_constraints (id, tile) =
    let ops = Fun.id :: Image.permutations in
    List.map (Fun.compose (Fun.flip Fun.(@@) tile) (tile_eq_constraint id))  ops
    |> or_

  let tile_constraints =
    let ids = List.map fst Input.data in
    let value_constraints = Input.data |> List.map tile_value_constraints in
    let assign_constriants = ids |> List.map tile_assign_constraint  in
    let assign_unique = ids |> List.map (fun id -> tile_variables.@[`Assign id]) |> unique in
    value_constraints, assign_constriants @ assign_unique

  let build_lookup_map model =
    let eval expr =
      Z3.Model.eval model expr true
      |> Option.get_exn
      |> Z3.Expr.to_string
      |> Int.of_string_exn in
    IntMap.to_iter tile_variables
    |> Iter.map (fun (id, (_,_,_,_, asn)) -> eval asn, id)
    |> IntMap.of_iter

  let solve () =
    let solver = Z3.Solver.mk_solver ctx None in
    Z3.Solver.add solver (base_image_constraints @ (fst tile_constraints) @ (snd tile_constraints));
    let result = Z3.Solver.check solver [] in
    let model = Z3.Solver.get_model solver |> Option.get_exn in
    let lookup_map = build_lookup_map model in
    model, lookup_map

  let (.&[]) map (x,y) =
    let ind = y * image_dim + x in
    IntMap.find ind map

  let corners (_, map) =
    [map.&[0, 0]; map.&[0, image_dim - 1];
     map.&[image_dim - 1, 0]; map.&[image_dim - 1, image_dim - 1]]

end


module Part1 = struct

  let read_id txt =
    List.nth (String.split_on_char ' ' txt) 1 |> drop_last |> Int.of_string_exn

  let read_tile txt =
    txt |> String.split_on_char '\n'
    |> fun ls -> let (id, body) = List.hd_tl ls in (read_id id,Image.read body)

  let read_input txt =
    txt |> String.split ~by:"\n\n" |> List.filter (Fun.negate String.is_empty)
    |> List.map read_tile

  let result txt =
    let data = read_input txt in
    let module S = Solver(struct  let data = data end) in
    let model = S.solve () in
    S.corners model |> List.fold_left Int.( * ) 1 

  let () = result fake_input |> print_int

end
