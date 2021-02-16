open Containers

let raw_input = IO.with_in "./day3/input" IO.read_all

module Map : sig
  type t = private int * bool array [@@deriving show]

  val dim : t -> int * int

  val (.@[]) : t -> int * int -> bool
  val (.@[]<-) : t -> int * int -> bool -> unit

  val of_str: ?char:char -> string -> t

end = struct
  type t = int * bool array [@@deriving show]

  let dim (a,arr) = (a,Array.length arr / a)

  let (.@[]) (cols,map) (row,col) =
    let ind = row * cols + col in
    Array.get map ind

  let (.@[]<-) (cols,map) (row,col) value =
    let ind = row * cols + col in
    Array.set map ind value

  let of_str ?(char='#') str =
    let lines = str
    |> String.split_on_char '\n' in
    let raw_data = lines
               |> Iter.of_list
               |> Iter.filter (Fun.negate String.is_empty)
               |> Iter.flat_map (fun str -> Iter.of_str str |> Iter.map (Char.equal char)) in
    let data = Iter.to_array @@ raw_data in
    let cols = match lines with h :: _ -> String.length h | _ -> invalid_arg "bad input." in
    (cols, data)

end


let input = raw_input
            |> Map.of_str

module Part1 = struct

let update map =
  let (cols,_) = Map.dim map in
  fun  (count, hpos) level -> 
  let open Map in
  let count = if map.@[(level, hpos)] then count + 1 else count in
  let hpos = (hpos + 3) mod cols in
  (count, hpos)

let result =
  let (_, rows) = Map.dim input in
  Iter.int_range ~start:0 ~stop:(rows - 1)
  |> Iter.fold (update input) (0,0)

end


module Part2 = struct

  type checker = {right:int; down:int}

  let update ({right;_}: checker) map =
    let (cols,_) = Map.dim map in
    fun  (count, hpos) level -> 
      let open Map in
      let count = if map.@[(level, hpos)] then count + 1 else count in
      let hpos = (hpos + right) mod cols in
      (count, hpos)

  let result ({down; _} as checker: checker) =
    let (_, rows) = Map.dim input in
    Iter.int_range_by ~step:down 0 (rows - 1)
    |> Iter.fold (update checker input) (0,0)
    |> fst

  let result =
    result {right=1;down=1} *
    result {right=3;down=1} *    
    result {right=5;down=1} *    
    result {right=7;down=1} *    
    result {right=1;down=2}

end
