open Containers


let raw_input = IO.with_in "./day7/input" IO.read_all


module StringMap =  CCMultiMap.Make (String) (String)
module StringGraph = struct
  include Map.Make (String)

  type nonrec 'a t = 'a list t

  let add map key value =
    update key (function
        | None -> Some [value]
        | Some ls -> Some (value :: ls)
      ) map


end
module StringSet =  Set.Make (String)




let fake_input = "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."

let remove_bag str = 
  if String.suffix ~suf:"bags" str
  then String.rtrim @@ String.sub str 0 (String.length str - 4)
  else if String.suffix ~suf:"bag" str
  then String.rtrim @@ String.sub str 0 (String.length str - 3)
  else assert false





let print_map map =
  StringMap.iter map (fun k v -> print_endline @@ Format.sprintf "%s -> %s;" k v)

module Part1 = struct

  let to_spec txt = String.sub txt 0 (String.length txt - 1)
                    |> String.split_on_char ','
                    |> Iter.of_list
                    |> Iter.map String.trim
                    |> Iter.map (String.drop_while CCParse.is_num)
                    |> Iter.map String.trim                
                    |> Iter.map remove_bag

  let to_relation txt = txt
                        |> String.split ~by:"contain"
                        |> (function
                            | [h; t] ->
                              (remove_bag @@ String.trim h, to_spec t)
                            | _ -> failwith "invalid input")

  let find_unique_ancestors key map =
    let rec find_unique_ancestors state key =
      let ancestors = StringMap.find map key in
      let state = List.fold_left (Fun.flip StringSet.add) state ancestors in
      List.fold_left find_unique_ancestors state ancestors in
    StringSet.to_list @@ find_unique_ancestors StringSet.empty key


  let result = raw_input
               |> String.split_on_char '\n'
               |> Iter.of_list
               |> Iter.filter (Fun.negate String.is_empty)
               |> Iter.map to_relation
               |> Iter.fold
                 (fun map (parent, children) ->
                    Iter.fold (fun map child -> StringMap.add map child parent) map children)
                 StringMap.empty
               |> find_unique_ancestors "shiny gold"
               |> List.length
end

module Part2 = struct

  let to_spec txt = String.sub txt 0 (String.length txt - 1)
                    |> String.split_on_char ','
                    |> Iter.of_list
                    |> Iter.map String.trim
                    |> Iter.map (fun str ->
                        String.to_list str
                        |> List.take_drop_while CCParse.is_num
                        |> (fun (num, rest) ->
                            Int.of_string @@ String.trim @@ String.of_list num
                            |> Option.get_or ~default:0,
                            remove_bag @@ String.trim @@ String.of_list rest))

  let to_relation txt = txt
                        |> String.split ~by:"contain"
                        |> (function
                            | [h; t] ->
                              (remove_bag @@ String.trim h, to_spec t)
                            | _ -> failwith "invalid input")  

  let rec find_subcount ?(cache=Hashtbl.create 10) key map =
    match Hashtbl.find_opt cache key with
    | Some vl -> vl
    | None ->
      match StringGraph.find_opt key map with
      | None -> 0
      | Some children ->
        let count = 
          Iter.of_list children
          |> Iter.map (fun (count, child) -> count + count * find_subcount ~cache child map)
          |> Iter.fold Int.(+) 0 in
        Hashtbl.add cache key count;
        count

  let result = raw_input
               |> String.split_on_char '\n'
               |> Iter.of_list
               |> Iter.filter (Fun.negate String.is_empty)
               |> Iter.map to_relation
               |> Iter.fold
                 (fun map (parent, children) ->
                    Iter.fold (Fun.flip StringGraph.add parent) map children)
                 StringGraph.empty
               |> find_subcount "shiny gold"
               |> print_int

end
