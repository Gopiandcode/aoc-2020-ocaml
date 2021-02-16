open Containers

module LList = Core_kernel.Doubly_linked
module IntMap = Hashtbl.Make (Int)

let raw_input = IO.with_in "./day23/input" IO.read_all
let fake_input = "389125467"

let read_cups txt =
  String.to_list txt |> List.map String.of_char |> List.filter_map Int.of_string |> List.map (Fun.flip (-) 1)

let to_string ls = ls |> List.map (Fun.compose ((+) 1)Int.to_string) |> String.concat ""

let rotate_to_one ls  =
  let (pre_1, one_and_after) = List.take_drop_while (function 0 -> false | _ -> true) ls in
  List.tl one_and_after @  pre_1 @ [0]

let play_game n cards =
  let len = List.length cards in
  let (+) a b = (a + b) mod len in
  let (-) a b = (len + (a - b)) mod len in
  let rotate = function [] -> failwith "empty list" | h :: t -> t @ [h] in
  let current_cup ls = List.hd ls in
  let draw_3_cups ls =
    List.take_drop 4 ls |> (function ([a;b;c;d], ls) -> [b;c;d], a::ls | _ -> failwith "") in
  let find_next_cup cups ls =
    let rec loop count =
      if List.mem count cups then loop (count - 1)
      else count in
    loop (current_cup ls - 1) in
  let insert_after pos cups =
    let rec loop acc = function
      | [] -> failwith @@ Printf.sprintf "reached end of input looking for %d" pos
      | h :: rest when h = pos -> List.rev (h :: acc) @ cups @ rest
      | h :: t -> loop (h :: acc) t in
    loop [] in
  let crab_move ls =
    let (cups, ls) = draw_3_cups ls in
    let next_cup = find_next_cup cups ls in
    insert_after next_cup cups ls |> rotate in
  Fun.iterate n crab_move cards

let rec to_seq (seq: 'a Core_kernel.Sequence.t) : 'a Seq.t =
  fun () -> 
  match Core_kernel.Sequence.next seq with
  | None -> Seq.Nil
  | Some (vl,nxt) ->
    Cons (vl, to_seq nxt)

let play_game_imperative ?size n cards =
  let len = match size with
    | None -> List.length cards
    | Some size -> size in
  let deck =
    Array.init len (fun ind -> if ind < List.length cards then List.nth cards ind else ind) |>
    LList.of_array in
  let elem_table =
    let tbl = IntMap.create len in
    LList.iter_elt deck ~f:(fun elt -> IntMap.add tbl (LList.Elt.value elt) elt);
    tbl in
  let (-) a b = (len + (a - b)) mod len in
  let rotate () = LList.move_to_back deck (LList.first_elt deck |> Option.get_exn) in
  let current_cup () = LList.first deck |> Option.get_exn in
  let current_cup_elt () = LList.first_elt deck  |> Option.get_exn in
  let next elt = LList.next deck elt |> Option.get_exn in
  let value elt = LList.Elt.value elt in
  let move_after ~elt after = LList.move_after deck ~anchor:after elt in
  let draw_3_cups () =
    let head = current_cup_elt () in
    let n1 = next head in
    let n2 = next n1 in
    let n3 = next n2 in
    n1,n2,n3 in
  let find_next_cup cups  =
    let rec loop count =
      if List.mem count cups
      then loop (count - 1)
      else count in
    loop (current_cup () - 1) in
  let crab_move () =
    let (n1,n2,n3) = draw_3_cups () in
    let next_cup = find_next_cup [value n1; value n2; value n3] in
    let next_cup = IntMap.find elem_table next_cup in
    move_after next_cup ~elt:n3;
    move_after next_cup ~elt:n2;
    move_after next_cup ~elt:n1;
    rotate () in
  Fun.iterate n crab_move ();
  let elem = IntMap.find elem_table in
  deck, elem

module Part1 = struct 

  let result txt = read_cups txt |> play_game 100 |> rotate_to_one |> to_string

  let () = result raw_input |> print_endline

end

module Part2 = struct

  let calculate_score (deck,elem) =
    let elem = elem 0 in
    let e1 = LList.next deck elem |> Option.get_exn in
    let e2 = LList.next deck e1 |> Option.get_exn in
    (LList.Elt.value e1 + 1) * (LList.Elt.value e2 + 1)

  let result txt =
    read_cups txt
    |> play_game_imperative ~size:1_000_000 10_000_000
    |> calculate_score

  let () =
    result raw_input |> Fun.compose Int.to_string print_endline

end
