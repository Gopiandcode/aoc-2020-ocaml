[@@@warning "-26"]
open Containers

module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let raw_input  = IO.with_in "./day20/input" IO.read_all

let empty_bitstring = Int.zero
let nth_bit n = (Int.shift_left Int.one n)
let get_bit n vl = Int.(land) (nth_bit n) vl > 0
let set_bit n vl = Int.(lor) (nth_bit n) vl
let count_bits n str =
  Iter.int_range ~start:0 ~stop:(n - 1)
  |> Iter.fold (fun count ind -> if get_bit ind str then count + 1 else count) 0 

let drop_last str = String.take (String.length str - 1) str
let drop_border ls =
  let drop_edge ls = List.take (List.length ls - 2) (List.tl ls) in
  drop_edge ls |> List.map (Fun.compose String.to_list drop_edge)

let transform (rot: [> `FL of int | `ID | `RL of int ]) img =
  let (.@[]) ls v = List.nth ls v in
  let flip_h = List.rev in
  let rotate ls  =
    let height = List.length ls and width = List.hd ls |> List.length in
    List.init width (fun row -> List.init height (fun col -> ls.@[ col].@[(width - row - 1)])) in
  (match rot with
   | `RL n -> (Fun.iterate n rotate) 
   | `ID -> Fun.id
   | `FL n -> Fun.compose (Fun.iterate n rotate) flip_h)  img

let all_transforms = [ `ID; `RL 1; `RL 2; `RL 3; `FL 0; `FL 1; `FL 2; `FL 3; ]

module Image = struct

  type row = Int.t

  type t = { size: int; top: int; right: int; bottom: int; left: int; } [@@deriving show, eq]

  let invert size row =
    let update str ind =
      if get_bit (size - ind - 1) row then set_bit ind str else str in
    Iter.int_range ~start:0 ~stop:(size - 1)
    |> Iter.fold update empty_bitstring

  let no_matching_dr t ls = List.count (fun v -> t.right = v || t.bottom = v) ls

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

  let flip_v img =
    {img with top=invert img.size img.top;
              bottom=invert img.size img.bottom;
              left=img.right;
              right=img.left; }

  let permutations = [
    Fun.id; rotate_left; rotate_left2; rotate_left3;
    flip_h; Fun.compose rotate_left flip_h;
    Fun.compose rotate_left2 flip_h; Fun.compose rotate_left3  flip_h; ]
  let labelled_permutations = [
    `ID, Fun.id; `RL 1, rotate_left; `RL 2, rotate_left2; `RL 3, rotate_left3;
    `FL 0, flip_h; `FL 1, Fun.compose rotate_left flip_h;
    `FL 2, Fun.compose rotate_left2 flip_h;
    `FL 3, Fun.compose rotate_left3 flip_h; ]

  let determine_transform ~original updated =
    List.find (fun (_,f) -> equal (f original) updated) labelled_permutations |> fst

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

module AdjacencyList = struct

  type t = int list IntMap.t * IntSet.t IntMap.t

  let build ls : t =
    let edge_map =
      List.map (fun (id, img) -> id, IntSet.of_list @@ Image.edges img) ls
      |> IntMap.of_list in
    let is_adjacent (ind, edge_set)  (other, other_set) =
      (not (other = ind)) && (not @@ (IntSet.inter edge_set other_set |> IntSet.is_empty)) in
    let find_adjacent_to vl =
      IntMap.to_iter edge_map |> Iter.filter (is_adjacent vl) |> Iter.map fst |> Iter.to_list in
    let map = IntMap.to_iter edge_map
              |> Iter.map (fun vl -> (fst vl), find_adjacent_to vl)
              |> IntMap.of_iter in
    (map, edge_map)

  let corner_tiles (t: t) =
    fst t |> IntMap.to_iter |> Iter.filter (fun (_ind,adjacent) -> List.length adjacent = 2)
    |> Iter.map fst |> Iter.to_list

  let adjacent_tiles tile  (t: t) =  IntMap.find tile (fst t) |> List.to_iter

  let adjacent_tiles_with_edge ~edge tile (t: t) =
    IntMap.find tile (fst t) |> Iter.of_list
    |> Iter.filter (Fun.compose (Fun.flip IntMap.find (snd t)) @@ IntSet.mem edge)

end

module TileMap = struct

  type coord = int * int
  module Grid = Map.Make (struct type t = int * int [@@deriving ord] end)

  type t = {
    seen:IntSet.t; grid: (int * Image.t) Grid.t;
    min_x: int; min_y: int; max_x: int; max_y: int; grid_size:int;
  } 

  let xrange t = (t.min_x, t.max_x)
  let yrange t = (t.min_y, t.max_y)

  let make ~grid_size (id,tile) =
    { seen=IntSet.singleton id; grid=Grid.singleton (0,0) (id,tile);
      min_x=0; max_x=0; min_y=0; max_y=0; grid_size; }

  let valid_tile t (id, _) = not (IntSet.mem id t.seen)

  let (.@[]) t (x,y) =
    if t.min_x <= x && x <= t.max_x && t.min_y <= y && y <= t.max_y
    then Grid.find_opt (x,y) t.grid else None

  let (.![]) t (x,y) = fst (Grid.find (x,y) t.grid)

  let (.&[]) t (x,y) = Grid.find (x,y) t.grid

  let all_cells_occupied t =
    let x_range = Iter.int_range ~start:t.min_x ~stop:(t.min_x + t.grid_size - 1) in
    let y_range = Iter.int_range ~start:t.min_y ~stop:(t.min_y + t.grid_size - 1) in
    not  (Iter.product x_range y_range
          |> Iter.map (fun coord -> t.@[coord] |> Option.is_some) |> Iter.exists (not))

  let is_complete t =
    if IntSet.cardinal t.seen = t.grid_size * t.grid_size
    then all_cells_occupied t
    else false

  let is_adjacent t (x,y) =
    Option.is_none t.@[(x,y)] &&
    List.exists (fun coord -> Option.is_some t.@[coord]) [(x-1,y); (x+1,y); (x,y-1); (x, y+1)] &&
    (if x < t.min_x then (t.max_x - x < t.grid_size) else (x - t.min_x < t.grid_size)) &&
    (if y < t.min_y then (t.max_y - y < t.grid_size ) else (y - t.min_y < t.grid_size))

  let check_valid t (x,y) tile =
    let tile_valid f v = Option.map snd v |> Option.map f |> Option.get_or ~default:true in
    let left_valid =
      t.@[(x-1,y)] |> tile_valid (fun left -> Image.matches_horiz left tile) in
    let right_valid =
      t.@[(x+1,y)] |> tile_valid (fun right -> Image.matches_horiz tile right) in
    let top_valid =
      t.@[(x,y-1)] |> tile_valid (fun top -> Image.matches_vert top tile) in
    let bottom_valid =
      t.@[(x,y+1)] |> tile_valid (fun bottom -> Image.matches_vert tile bottom) in
    let coord_valid =
      let x_valid = 
        if x < t.min_x then t.max_x - x < t.grid_size else x - t.min_x < t.grid_size in
      let y_valid = 
        if y < t.min_y then t.max_y - y < t.grid_size else y - t.min_y < t.grid_size in
      x_valid && y_valid in
    coord_valid && left_valid && right_valid && top_valid && bottom_valid

  let add_tile t  (id,tile) ((x,y): coord) =
    if IntSet.mem id t.seen || not (check_valid t (x,y) tile) || Option.is_some t.@[(x,y)]
    then None
    else Some { t with seen=IntSet.add id t.seen; grid=Grid.add (x,y) (id,tile) t.grid;
                       min_x=min x t.min_x; min_y=min y t.min_y;
                       max_x=max x t.max_x; max_y=max y t.max_y; }
end

module Context = struct

  type t = {
    tiles: Image.t IntMap.t;
    adj_map: AdjacencyList.t;
    grid_size: int;
  }

  let read_id txt =
    List.nth (String.split_on_char ' ' txt) 1 |> drop_last |> Int.of_string_exn

  let read_tile txt =
    txt |> String.split_on_char '\n'
    |> fun ls -> let (id, body) = List.hd_tl ls in (read_id id,Image.read body)

  let read_input txt =
    txt |> String.split ~by:"\n\n" |> List.filter (Fun.negate String.is_empty)
    |> List.map read_tile

  let corners ctx = AdjacencyList.corner_tiles ctx.adj_map 

  let grid_size ctx = ctx.grid_size

  let image ctx base_tile = IntMap.find base_tile ctx.tiles
  let adjacent ctx tile = AdjacencyList.adjacent_tiles tile ctx.adj_map

  let make txt =     
    let data = read_input txt in
    let grid_size = List.length data |> Float.of_int |> sqrt |> Int.of_float in
    let tiles = IntMap.of_list data in
    let adj_map = AdjacencyList.build data in
    {grid_size; tiles; adj_map}

end

module BinaryPattern = struct

  type t = {width: int; height: int; size: int; data: int list list}
  type s = int * int * int * int array

  let to_array {width; height; data; size; _} =
    (size, width, height, List.flatten data |> Array.of_list)

  let (.&[]) ((_, width, _, arr): s) (x,y) = arr.(width * y + x)

  let (.![]) ({data; _}: t) (x,y) = List.nth (List.nth data y) x

  let mask_matches mask vl = (mask land vl) = mask
  let unset_matches mask vl = assert (mask_matches mask vl); mask lxor vl

  let to_int txt =
    List.foldi (fun str ind -> function
          ' ' -> str
        | '#' -> set_bit ind str
        | c -> failwith (Printf.sprintf "unknown char %c" c)) empty_bitstring txt

  let make size (ls: char list list) =
    let height = List.length ls in
    let data = List.map (List.sublists_of_len ~last:Option.return size) ls in
    let width = List.length (List.hd data) in
    let data = List.map (List.map to_int) data in
    {data; width; height; size}

  let mask size = Iter.int_range ~start:0 ~stop:(size - 1)
                  |> Iter.fold (Fun.flip set_bit) empty_bitstring 

  let shift n t =
    assert (n <= t.size);
    let non_empty_seen = ref false in
    let mask = mask t.size in
    let shift_by ls =
      let update prev_str value =
        let value = Int.shift_left value n in
        let next_str = (Int.shift_right value t.size) in
        let new_value = Iter.int_range ~start:0 ~stop:(n - 1)
                        |> Iter.fold (fun str ind ->
                            if get_bit ind prev_str
                            then set_bit ind str
                            else str) (value land mask) in
        (next_str, new_value) in
      let (result, ls) = List.fold_map update empty_bitstring ls in
      if not (result = empty_bitstring) then non_empty_seen := true;
      ls @ [result] in
    if n = 0
    then t
    else
      let drop_last ls = List.take (List.length ls - 1) ls in
      let data = List.map shift_by t.data in
      if !non_empty_seen
      then {t with data; width = t.width + 1}
      else {t with data=List.map drop_last data; width = t.width}

  let of_string ?(tr=`ID) size str =
    str |> String.split_on_char '\n' |> List.map String.to_list |> transform tr |> make size

  let monster_txt =
"                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "

  let monster ?(tr=`ID) size = monster_txt |> of_string ~tr size

  let all_monsters size =
    let base_monster tr = monster_txt |> of_string ~tr size in
    List.map (fun tr -> base_monster tr) all_transforms

end

module BinaryMap = struct

  type t = { width: int; height: int; size: int; data: int array}

  let to_int txt =
    List.foldi (fun str ind -> function
          '.' -> str
        | '#' -> set_bit ind str
        | c -> failwith (Printf.sprintf "unknown char '%c'" c)) empty_bitstring txt

  let (.@[]) ({width; data; _}: t) (x,y) = data.(width * y + x)

  let (.@[]<-) ({width; data; _}: t) (x,y) vl = data.(width * y + x) <- vl

  let pattern_matches (t: t) (bx,by) ((_, width, height, _) as pat: BinaryPattern.s) =
    if bx + width <= t.width && by + height <= t.height
    then
      let open BinaryPattern in
      let exception Mismatch in
      try
        for y = 0 to height - 1 do
          for x = 0 to width - 1 do
            if not @@ mask_matches pat.&[(x,y)] t.@[(bx + x, by + y)]
            then raise Mismatch
          done
        done;
        true
      with Mismatch -> false
    else false

  let remove_pattern (t: t) (bx,by) ((_, width, height, _) as pat: BinaryPattern.s) =
    let t = {t with data = Array.copy t.data} in
    if bx + width <= t.width && by + height  <= t.height
    then 
      let open BinaryPattern in
      for y = 0 to height - 1 do
        for x = 0 to  width - 1 do
          t.@[(bx + x,by + y)] <- unset_matches pat.&[(x,y)] t.@[(bx + x, by + y)]
        done
      done;
      t
    else assert false

  let remove_monsters (t: t) (pat: BinaryPattern.t) =
    let any_removed = ref false in
    let height, width = t.height, t.width in
    let t = ref t in
    for offset = 0 to pat.size - 1 do
      let pat = BinaryPattern.shift offset pat |> BinaryPattern.to_array in
      for y = 0 to height - 1 do
        for x = 0 to width - 1 do
          if pattern_matches !t (x,y) pat
          then begin
            t := remove_pattern !t (x,y) pat;
            any_removed := true
          end
        done
      done
    done;
    if !any_removed
    then Some !t
    else None

  let count_bits t = t.data |> Array.fold (fun acc str -> acc + count_bits t.size str) 0

  let build ctx to_characters complete_map =
    let grid_size = Context.grid_size ctx in
    let len = ref 0 in
    let data = 
      Iter.int_range ~start:0 ~stop:((grid_size * grid_size) - 1)
      |> Iter.map (fun ind ->
          let tile, final_image =
            let y = ind / grid_size in
            let x = ind - grid_size * y in
            complete_map.TileMap.&[(x,y)] in
          let to_ints ls = List.map (Image.read_row (List.length ls)) ls in
          let rot = Image.determine_transform ~original:(Context.image ctx tile) final_image in
          let tile_map = to_characters tile |> drop_border |> transform rot in
          len := List.length tile_map;
          tile_map   |> to_ints |> Array.of_list)
      |> Iter.to_array in
    let range n = Iter.int_range ~start:0 ~stop:(n - 1) in
    let data =
      (range grid_size) |> Iter.flat_map (fun row ->
          range !len |> Iter.flat_map (fun sub_column ->
              range grid_size |> Iter.map (fun col ->
                  (data.(row * grid_size + col)).(sub_column))))
      |> Iter.to_array in
    let width = grid_size in
    let height = grid_size * !len in
    {data; width; height; size = !len}

end

module Part1 = struct

  let result txt =
    let ctx = Context.make txt  in
    Context.corners ctx
    |> List.fold_left Int.( * ) 1

  let () = result raw_input |> Fun.compose Int.to_string print_endline

end

module Part2 = struct

  let read_tile_raw txt =
    let read_id txt =
      List.nth (String.split_on_char ' ' txt) 1
      |> drop_last
      |> Int.of_string_exn in
    txt |> String.split_on_char '\n'
    |> fun ls -> let (id,body) = List.hd_tl ls in (read_id id,body)

  let read_input_raw txt =
    txt |> String.split ~by:"\n\n" |> List.filter (Fun.negate String.is_empty)
    |> List.map read_tile_raw


  let all_permutations ctx base_tile =   
    let image = Context.image ctx base_tile in
    (base_tile, image) :: (List.map (fun f -> (base_tile, f image))  Image.permutations)
    |> List.to_iter

  let all_tiles data = Iter.of_list data |> Iter.map fst

  let next_pos ctx (i,j) =
    if j + 1 < Context.grid_size ctx then Some (`Below, (i,j+1))
    else if i + 1 < Context.grid_size ctx then Some (`Restart, (i+1,0))
    else None

  let populate_map ctx base_tile last_pos map =
    let rec loop base_tile last_pos map = 
      match next_pos ctx last_pos with
      | None -> Some map
      | Some (spec, new_pos) ->
        let tile = match spec with
            `Below -> fst base_tile
          | `Restart -> TileMap.(map.![(fst new_pos - 1, snd new_pos)]) in
        Context.adjacent ctx tile |> Iter.find_map (fun tile ->
            Iter.find_map (fun new_tile ->
                match TileMap.add_tile map new_tile new_pos with
                | None -> None
                | Some map -> loop new_tile new_pos map) (all_permutations ctx tile)) in
    loop base_tile last_pos map

  let result txt = 
    let characters =
      let map = read_input_raw txt |> IntMap.of_list in
      fun tile -> IntMap.find tile map in
    let ctx = Context.make txt in
    let map =
      let corner_tiles =
        Context.corners ctx |> Iter.of_list |> Iter.flat_map (all_permutations ctx) in
      Iter.find_map
        (fun base_tile ->
           let map = TileMap.make ~grid_size:ctx.grid_size base_tile in
           populate_map ctx base_tile (0,0) map) corner_tiles
      |> Option.get_exn in
    let binary_map = BinaryMap.build ctx characters map in
    Iter.of_list (BinaryPattern.all_monsters binary_map.size)
    |> Iter.find (BinaryMap.remove_monsters binary_map)
    |> Option.get_exn
    |> BinaryMap.count_bits

  let () = result raw_input |> Fun.compose Int.to_string print_endline

end
