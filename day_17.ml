open Containers

let raw_input = IO.with_in "./day17/input" IO.read_all

type cell = Active | Inactive [@@deriving show]

module CellUpdate (Space: sig
  type coordinate
  val dims: int
  val (@+) : coordinate -> coordinate -> coordinate
  val is_zeros: coordinate -> bool
  val to_coord: int list -> coordinate
  val lookup : coordinate -> cell
end) = struct

  let permutations ls  =
    let rec loop acc = function
      | [] -> acc
      | h :: t -> loop (List.flat_map (fun vl -> List.map (List.cons vl) acc) h) t in
    loop [[]] (List.rev ls)

  let neighborhood base_point =
    let updates = 
      let pm1 = [1;0;-1] in
      let directions = List.replicate Space.dims pm1 in
      permutations directions
      |> List.map Space.to_coord
      |> List.filter (Fun.negate Space.is_zeros) in
    List.map (Space.(@+) base_point) updates

  let update_cell point cell =
    let neighborhood = neighborhood point in
    let no_active =
      List.count (Fun.compose Space.lookup
                    (function Active -> true | _ -> false)) neighborhood in
    match cell with
    | Active -> if no_active = 2 || no_active = 3 then Active else Inactive
    | Inactive -> if no_active = 3 then Active else Inactive

end

module Grid3 = struct

  type coordinate = int * int * int
  module TupleMap = Map.Make(struct type t = int * int * int [@@deriving ord] end)

  type t = {
    min_x:int; max_x:int;
    min_y:int; max_y:int;
    min_z:int; max_z:int;
    data: cell TupleMap.t
  }

  let lookup t pair =
    match TupleMap.find_opt pair t.data with
    | None -> Inactive
    | Some state -> state

  let read_cell = function | '#' -> Active | '.' -> Inactive | _ -> failwith "invalid input"

  let read_row txt = String.to_list txt |> List.map read_cell

  let find_range (min_x, min_y, min_z, max_x, max_y, max_z) (x,y,z)  =
    (min min_x x, min min_y y, min min_z z,
     max max_x x, max max_y y, max max_z z)

  let read_input txt =
    let rows = String.split_on_char '\n' txt
               |> List.filter (Fun.negate String.is_empty)
               |> List.map read_row in
    let data = List.foldi (fun map row data ->
        List.foldi (fun map col data ->
            TupleMap.add (col, row, 0) data map
          ) map data
      ) TupleMap.empty rows in
    let (min_x, min_y, min_z, max_x, max_y, max_z) =
      TupleMap.keys data |> Iter.fold find_range (0,0,0, 0,0,0) in
    { min_x = min_x - 1; min_y = min_y - 1; min_z = min_z - 1;
      max_x = max_x + 1; max_y = max_y + 1; max_z = max_z + 1;
      data }

  let iter_cells t = TupleMap.values t.data

  let update t f =
    let min_x, min_y, min_z = ref t.min_x, ref t.min_y, ref t.min_z in
    let max_x, max_y, max_z = ref t.max_x, ref t.max_y, ref t.max_z in
    let update_data map (x,y,z) =
      let cell = lookup t (x,y,z) in
      let cell = f (x,y,z) cell in
      begin match cell with
        | Inactive -> ()
        | Active ->
          min_x := min !min_x (x - 1); min_y := min !min_y (y - 1); min_z := min !min_z (z - 1);
          max_x := max !max_x (x + 1); max_y := max !max_y (y + 1); max_z := max !max_z (z + 1);
      end;
      TupleMap.add (x,y,z) cell map in
    let z_range = Iter.int_range ~start:t.min_z ~stop:t.max_z in
    let y_range = Iter.int_range ~start:t.min_y ~stop:t.max_y in
    let x_range = Iter.int_range ~start:t.min_x ~stop:t.max_x in
    let data = Iter.product x_range y_range
    |> Fun.flip Iter.product z_range
    |> Iter.map (fun ((a,b),c) -> (a,b,c))
    |> Iter.fold update_data t.data in
    { data; min_x= !min_x;min_y= !min_y;min_z= !min_z;
      max_x= !max_x;max_y= !max_y;max_z= !max_z; }
end

module Grid4 = struct

  type coordinate = int * int * int * int
  module TupleMap = Map.Make(struct type t = int * int * int * int [@@deriving ord] end)

  type t = {
    min_x:int; max_x:int;
    min_y:int; max_y:int;
    min_z:int; max_z:int;
    min_w:int; max_w:int;
    data: cell TupleMap.t
  }

  let lookup t pair =
    match TupleMap.find_opt pair t.data with
    | None -> Inactive
    | Some state -> state

  let read_cell = function | '#' -> Active | '.' -> Inactive | _ -> failwith "invalid input"

  let read_row txt = String.to_list txt |> List.map read_cell

  let find_range (min_x, min_y, min_z, min_w, max_x, max_y, max_z,max_w) (x,y,z,w)  =
    (min min_x x, min min_y y, min min_z z, min min_w w,
     max max_x x, max max_y y, max max_z z, max max_w w)

  let read_input txt =
    let rows = String.split_on_char '\n' txt
               |> List.filter (Fun.negate String.is_empty)
               |> List.map read_row in
    let data = List.foldi (fun map row data ->
        List.foldi (fun map col data ->
            TupleMap.add (col, row, 0, 0) data map
          ) map data
      ) TupleMap.empty rows in
    let (min_x, min_y, min_z, min_w, max_x, max_y, max_z, max_w) =
      TupleMap.keys data |> Iter.fold find_range (0,0,0,0, 0,0,0,0) in
    {
      min_x = min_x - 1; min_y = min_y - 1; min_z = min_z - 1; min_w = min_w - 1;
      max_x = max_x + 1; max_y = max_y + 1; max_z = max_z + 1; max_w = max_w + 1;
      data
    }

  let iter_cells t = TupleMap.values t.data

  let update t f =
    let min_x, min_y, min_z, min_w = ref t.min_x, ref t.min_y, ref t.min_z, ref t.min_w in
    let max_x, max_y, max_z, max_w = ref t.max_x, ref t.max_y, ref t.max_z, ref t.max_w in
    let update_data map (x,y,z,w) =
      let cell = lookup t (x,y,z,w) in
      let cell = f (x,y,z,w) cell in
      begin match cell with
        | Inactive -> ()
        | Active ->
          min_x := min !min_x (x - 1); max_x := max !max_x (x + 1);
          min_y := min !min_y (y - 1); max_y := max !max_y (y + 1);
          min_z := min !min_z (z - 1); max_z := max !max_z (z + 1);
          min_w := min !min_w (w - 1); max_w := max !max_w (w + 1);
      end;
      TupleMap.add (x,y,z,w) cell map in
    let w_range = Iter.int_range ~start:t.min_w ~stop:t.max_w in
    let z_range = Iter.int_range ~start:t.min_z ~stop:t.max_z in
    let y_range = Iter.int_range ~start:t.min_y ~stop:t.max_y in
    let x_range = Iter.int_range ~start:t.min_x ~stop:t.max_x in
    let data = Iter.product x_range y_range
    |> Fun.flip Iter.product z_range
    |> Fun.flip Iter.product w_range
    |> Iter.map (fun (((a,b),c),d) -> (a,b,c,d))
    |> Iter.fold update_data t.data in
    { data; min_x= !min_x;min_y= !min_y;min_z= !min_z; min_w = !min_w;
      max_x= !max_x;max_y= !max_y;max_z= !max_z; max_w = !max_w; }
end

module Part1 = struct

  let update state =
    let module Update = CellUpdate(struct
        type coordinate = Grid3.coordinate
        let (@+) (a1,b1,c1) (a2,b2,c2) = (a1 + a2, b1 + b2, c1 + c2)
        let to_coord = function [a1;a2;a3] -> (a1,a2,a3) | _ -> assert false
        let dims = 3
        let is_zeros = function (0,0,0) -> true | _ -> false
        let lookup = Grid3.lookup state
      end) in
    Grid3.update state Update.update_cell

  let result input =
    let init_state = Grid3.read_input input in
    Fun.iterate 6 update init_state
    |> Grid3.iter_cells
    |> Iter.fold (fun count -> function Active -> count + 1 | _ -> count) 0

end

module Part2 = struct

  let update state =
    let module Update = CellUpdate(struct
        type coordinate = Grid4.coordinate
        let dims = 4
        let (@+) (a1,b1,c1,d1) (a2,b2,c2,d2) = (a1 + a2, b1 + b2, c1 + c2, d1+d2)
        let to_coord = function [a1;a2;a3;a4] -> (a1,a2,a3,a4) | _ -> assert false
        let is_zeros = function (0,0,0,0) -> true | _ -> false
        let lookup = Grid4.lookup state
      end) in
    Grid4.update state Update.update_cell

  let result input =
    let init_state = Grid4.read_input input in
    Fun.iterate 6 update init_state
    |> Grid4.iter_cells
    |> Iter.fold (fun count -> function Active -> count + 1 | _ -> count) 0

end
