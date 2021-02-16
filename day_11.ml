open Containers


let raw_input = IO.with_in "./day11/input" IO.read_all
let fake_input = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"
let input = raw_input
            |> String.split_on_char '\n'
            |> List.filter (Fun.negate String.is_empty)


module Map : sig
  type cell = Empty | Occupied | Floor
  type t

  val from_string : string -> t
  val ( .@[] ) : t -> int * int -> cell

  val pp : Format.formatter -> t -> unit

  val update : (t -> int * int -> cell -> cell) -> t -> t

  val update_till_fixpoint : (t -> int * int -> cell -> cell) -> t -> t

  val fold_cells : ('a -> cell -> 'a) -> 'a -> t -> 'a

  val neighbourhood : int * int -> (int * int) list

  val up_left : int * int -> int * int
  val up : int * 'a -> int * 'a
  val up_right : int * int -> int * int
  val right : 'a * int -> 'a * int
  val down_right : int * int -> int * int
  val down : int * 'a -> int * 'a
  val down_left : int * int -> int * int
  val left : 'a * int -> 'a * int

  val repeat : t -> int * int -> (int * int -> int * int) -> cell Iter.t
  val no_occupied : t -> int * int -> int
end = struct
  type cell =
    | Empty
    | Occupied
    | Floor [@@deriving eq]

  let pp_cell fmt = function
    | Empty -> Format.pp_print_char fmt 'L'
    | Occupied -> Format.pp_print_char fmt '#'
    | Floor -> Format.pp_print_char fmt '.' 

  type t = {
    rows: int;
    cols: int;
    data: cell array;
  } [@@deriving eq]


  let from_string str =
    let data = str |> String.split_on_char '\n' |> List.filter (Fun.negate String.is_empty) in
    let cols = List.hd data |> String.length in
    let rows = List.length data in
    let data = Iter.of_list data
               |> Iter.flat_map Iter.of_str
               |> Iter.map (function
                   | 'L' -> Empty
                   | '#' -> Occupied
                   | '.' -> Floor
                   | _ -> failwith "invalid input")
               |> Iter.to_array in
    {rows; cols; data}

  let (.@[]) {rows;cols;data} (row,col) =
    if row < 0 || row >= rows || col < 0 || col >= cols
    then Floor
    else Array.unsafe_get data (cols * row + col)

  let (.@[]<-) {rows;cols;data} (row,col) vl =
    if row < 0 || row >= rows || col < 0 || col >= cols
    then failwith "assignment to element outside of array"
    else Array.unsafe_set data (cols * row + col) vl

  let pp fmt t =
    Format.pp_print_string fmt "Map {";
    Format.pp_open_vbox fmt 4;
    Format.pp_print_newline fmt ();
    for i = 0 to t.rows-1 do
      for j = 0 to t.cols-1 do
        pp_cell fmt (t.@[(i,j)])
      done;
      Format.pp_print_newline fmt ()
    done;
    Format.pp_close_box fmt ();
    Format.pp_print_string fmt "}"


  let update f t =
    let t' = {t with data=Array.make (Array.length t.data) Empty} in
    for i=0 to t.rows-1 do
      for j=0 to t.cols-1 do
        t'.@[(i,j)] <- f t (i,j) t.@[(i,j)]
      done
    done;
    t'

  let rec update_till_fixpoint f t =
    let t' = update f t in
    if equal t t'
    then t'
    else update_till_fixpoint f t'


  let fold_cells f init {data;_} = Array.fold f init data


  let neighbourhood (i,j) =
    [(i-1,  j); (i-1,j+1); (i  ,j+1); (i+1,j+1); (i+1,  j); (i+1,j-1); (i  ,j-1); (i-1,j-1)]



  let up (i,j) = (i - 1, j)
  let down (i,j) = (i + 1, j)
  let left (i,j) = (i, j - 1)
  let right (i,j) = (i, j + 1)
  let up_left = Fun.compose up left
  let up_right = Fun.compose up right
  let down_left = Fun.compose down left
  let down_right = Fun.compose down right

  let repeat map (i,j) f =
    let x = ref i in
    let y = ref j in
    let get_next () =
      let (new_x, new_y) = f (!x,!y) in
      x := new_x; y := new_y;
      if new_x < 0 || new_x >= map.rows ||  new_y < 0 || new_y >= map.cols
      then None
      else  Some map.@[(new_x, new_y)] in
    Iter.from_fun get_next

  let no_occupied map (i,j) =
    let is_occupied = function Occupied -> true | _ -> false in
    neighbourhood (i,j)
    |> Iter.of_list
    |> Iter.filter_count (fun coords -> (is_occupied map.@[coords]))

end


module Part1 = struct
  let update map (i,j) = function
    | Map.Empty when Map.no_occupied map (i,j) = 0 -> Map.Occupied
    | Map.Occupied when Map.no_occupied map (i,j) >= 4 -> Map.Empty
    | v -> v 

  let no_occupied_overall map =
    Map.fold_cells (fun count -> function Occupied -> count + 1 | _ -> count) 0 map

  let result input =
    Map.from_string input
    |> Map.update_till_fixpoint update
    |> no_occupied_overall
end


module Part2 = struct

  let directions =
    Map.[ up_left; up; up_right; right; down_right; down; down_left; left; ] 
  
  let is_occupied_in_direction it =
    let is_occupied = function Map.Empty | Map.Occupied -> true | _ -> false in
    match Iter.find_pred is_occupied it with
    | Some Map.Occupied -> true
    | _ -> false

  let no_occupied map (i,j) =
    let to_iter = Map.repeat map (i,j) in
    directions
    |> Iter.of_list
    |> Iter.map to_iter
    |> Iter.filter_count is_occupied_in_direction

  let update map (i,j) = function
    | Map.Empty when no_occupied map (i,j) = 0 -> Map.Occupied
    | Map.Occupied when no_occupied map (i,j) >= 5 -> Map.Empty
    | v -> v 
  
  let result input =
    Map.from_string input
    |> Map.update_till_fixpoint update
    |> Part1.no_occupied_overall

end
