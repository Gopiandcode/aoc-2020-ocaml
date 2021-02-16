open Containers

module Queue = CCFQueue

let raw_input = IO.with_in "./day22/input" IO.read_all

let read_title txt =
  let rule = Parse.(string "Player" *> char ' ' *> chars1_if is_num <* char ':') in
  Parse.parse_string_exn rule txt |> Int.of_string_exn

let read_deck txt =
  let (title, cards) = String.split_on_char '\n' txt |>  List.hd_tl in
  let title = read_title title in
  let cards = List.filter_map Int.of_string cards in
  title,cards

let input txt =
  String.split ~by:"\n\n" txt |> List.map read_deck |> List.hd_tl |> Pair.map_snd List.hd

let print_state (p1, p2) =
  let print_state (id, ls) =
    print_endline @@
    Format.sprintf "Player %d's deck: %s" id
      (Format.pp_open_hbox Format.str_formatter ();
       (List.pp Int.pp) Format.str_formatter ls;
       Format.pp_close_box Format.str_formatter ();
       Format.flush_str_formatter ()) in
  print_state p1;
  print_state p2
  
let calculate_score ?player =
  let calculate_score ls =
    ls |> List.rev |> Iter.of_list
    |> Iter.zip_i |> Iter.map (fun (i,c) -> (i + 1) * c) |> Iter.fold (+) 0 in
  function
  | (ls), ([]) 
  | ([]), (ls) ->
    calculate_score ls
  | _, ( ls) when (match player with Some `P2 -> true | _ -> false)  ->
    calculate_score ls
  | ( ls), _ when (match player with Some `P1 -> true | _ -> false) ->
    calculate_score ls
  | _ -> failwith "game has not completed "

module Part1 = struct

  let step (p1, p2) =
    let (let+) x f = Option.bind x f in
    let get_next_card = function (_, []) -> None | (id, h :: t) -> Some ((id,t), h) in
    let add_cards cards p = Pair.map_snd (Fun.flip (@) cards) p in
    let+ (p1, p1_card) = get_next_card p1 in
    let+ (p2, p2_card) = get_next_card p2 in 
    if p1_card > p2_card
    then Some (add_cards [p1_card; p2_card] p1, p2)
    else if p2_card > p1_card
    then Some (p1, add_cards [p2_card; p1_card] p2)
    else  Some (add_cards [p1_card] p1, add_cards [p2_card] p2)

  let play_game state =
    let rec loop state =
      Option.get_lazy (fun () -> loop state) (step state)
    in loop state

  let result txt =  input txt |> play_game |> Pair.map_same snd |> calculate_score

  let print_result () = result raw_input |> Fun.compose Int.to_string print_endline

end

module Part2 = struct

  type 'a queue = 'a Queue.t

  let compare_queue f a b = List.compare f (Queue.to_list a) (Queue.to_list b)

  module StateSet = Set.Make (struct
      type t = int queue * int queue [@@deriving ord]
    end)

  let rec play_game (p1, p2) =
    let get_next_card ls = Queue.take_front ls in
    let deck_size ls = Queue.size ls in
    let add_cards cards vl = Queue.add_iter_back vl (List.to_iter cards) in
    let take_cards no ls = Queue.take_front_l no ls |> fst |> Queue.of_list in
    let rec loop seen_states (p1,p2) =
      if StateSet.mem (p1,p2) seen_states
      then `P1, (p1,p2)
      else
        let seen_states = StateSet.add (p1, p2) seen_states in
        match get_next_card p1, get_next_card p2 with
        | None, None -> failwith "invalid state"
        | None, Some _ -> `P2, (p1,p2)
        | Some _, None -> `P1, (p1, p2)
        | Some (p1_card, p1), Some (p2_card, p2) ->
          if deck_size p1 >= p1_card && deck_size p2 >= p2_card
          then
            let (p1,p2) = match play_game (take_cards p1_card p1, take_cards p2_card p2) with
              | `P1, _ -> (add_cards [p1_card; p2_card] p1, p2)
              | `P2, _ -> (p1, add_cards [p2_card; p1_card] p2) in
            loop seen_states (p1,p2)
          else match Int.compare p1_card p2_card with
            | -1 -> loop seen_states (p1, add_cards [p2_card; p1_card] p2)
            | 1 -> loop seen_states (add_cards [p1_card; p2_card]  p1, p2)
            | _ -> failwith "ambiguous" in
    loop StateSet.empty (p1,p2)

  let result txt =
    input txt
    |> Pair.map_same snd
    |> Pair.map_same Queue.of_list
    |> play_game
    |> Pair.map_snd (Pair.map_same Queue.to_list)
    |> (fun (p, v) -> calculate_score ~player:p v)

  let () = result raw_input |> Fun.compose Int.to_string print_endline

end
