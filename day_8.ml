open Containers


let raw_input = IO.with_in "./day8/input" IO.read_all
let fake_input = "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"


type instruction =
  | Acc of int
  | Jmp of int
  | Nop of int [@@deriving show]

type exec_state = {instr: int; acc: int}  [@@deriving show]
let initial_exec_state = {instr=0; acc=0;}

let jump offset exec_state = {exec_state with instr=exec_state.instr + offset}
let acc by exec_state = {acc=exec_state.acc + by; instr=exec_state.instr + 1}


let current_instruction code exec_state =
  Array.get code exec_state.instr

let step code exec_state =
  match current_instruction code exec_state with
  | Acc v -> acc v exec_state
  | Jmp v -> jump v exec_state
  | Nop _ -> jump 1 exec_state 

let read_value txt =
  let txt = txt |> String.trim in
  match String.get txt 0 with
  | '+' -> Int.of_string_exn (String.drop 1 txt)
  | '-' -> - Int.of_string_exn (String.drop 1 txt)
  | _ -> failwith "Invalid input"

let read_instr txt =
  txt
  |> String.trim 
  |> String.split_on_char ' '
  |> function
  | ["nop"; vl] -> Nop (read_value vl)
  | ["acc"; vl] -> Acc (read_value vl)
  | ["jmp"; vl] -> Jmp (read_value vl)
  | str -> failwith (Format.sprintf "%s - %s" "invalid input" (List.to_string Fun.id str))

let code =
  raw_input
  |> String.split_on_char '\n'
  |> Iter.of_list
  |> Iter.filter (Fun.negate String.is_empty)
  |> Iter.map read_instr
  |> Iter.to_array


module Part1 = struct
  type seen_instructions = bool array

  exception Repeat of int


  let find_acc_on_repeat code =
    let seen_instructions = Array.make (Array.length code) false in

    let check_state exec_state =
      if Array.unsafe_get seen_instructions exec_state.instr
      then raise (Repeat exec_state.acc)
      else Array.unsafe_set seen_instructions exec_state.instr true in
    let rec loop exec_state =
      let exec_state = step code exec_state in
      check_state exec_state;
      loop exec_state in

    try
      ignore @@ loop initial_exec_state;
      failwith "never repeated"
    with
    | Repeat acc -> acc

  let print_result () =
    find_acc_on_repeat code |> print_int


end

module Part2 = struct

  type bbinstr =
    | BAcc of int
    | BNop of int [@@deriving show, eq, ord]

  type bblock = {
    start: int;
    code: bbinstr list;
    nxt: int option;
  } [@@deriving show, ord, eq]

  type result = bblock list [@@deriving show]

  module Block = struct
    type t = bblock [@@deriving eq, ord]
    let hash = Hashtbl.hash
  end

  module G = struct
    open Graph

    include Imperative.Digraph.Concrete (Block)
  end

  module BlockSet = Set.Make (Block)

  let extract_basic_blocks (ls: instruction array) =
    let initial_state =
      let jump_targets =
        Iter.of_array ls
        |> Iter.zip_i
        |> Iter.filter_map (function
            |  (line_no, Jmp offset) -> Some (line_no + offset)
            | _ -> None
          ) |> Iter.sort_uniq |> Iter.to_list in
      jump_targets, [], [], 0 in
    let result =
      Iter.of_array ls
      |> Iter.zip_i
      |> Iter.fold
        (fun (jump_targets, blocks, current_block, current_block_start) (line_no, code) ->
           match jump_targets,code with
           | [], Acc v -> ([], blocks, BAcc v :: current_block, current_block_start)
           | [], Nop v -> ([], blocks, BNop v :: current_block, current_block_start)
           | [], Jmp offset -> ([], {
               code=List.rev current_block; nxt=Some (line_no + offset);
               start=current_block_start;
             } :: blocks, [], line_no + 1)
           | target :: rest, code when target = line_no ->
             let blocks =
               match current_block with
               | [] -> blocks
               | _ -> {
                   code=List.rev current_block; nxt=None; start=current_block_start
                 } :: blocks in
             let (blocks, current_block, current_block_start) = match code with
               | Acc v -> (blocks, [BAcc v], line_no)
               | Nop v -> (blocks, [BNop v], line_no)
               | Jmp offset ->
                 ({code=[]; nxt=Some (line_no + offset); start=line_no} :: blocks, [], line_no + 1) in
             (rest, blocks, current_block, current_block_start) 
           | jump_targets, Acc v ->
             (jump_targets, blocks, BAcc v :: current_block, current_block_start)
           | jump_targets, Nop v ->
             (jump_targets, blocks, BNop v :: current_block, current_block_start)
           | jump_targets, Jmp offset ->
             (jump_targets,
              {
                code=List.rev current_block; nxt=Some(line_no + offset);
                start=current_block_start;
              } :: blocks, [], line_no +1)
        ) initial_state in
    let final_instr = Array.length ls in
    let final_block = 
      {code=[]; nxt=None; start=final_instr} in
    let basic_blocks = match result with
      | (_, basic_blocks, [], _) ->
        final_block :: basic_blocks
      | (_, basic_blocks, last_block, current_block_start) ->
        final_block ::
        ({code=List.rev last_block; nxt=None; start=current_block_start} ::
         basic_blocks) in
    List.rev basic_blocks

  module BlockTable : sig
    type t 

    val start_block : t -> bblock
    val last_block : t -> bblock

    val build : bblock list -> t
    val length : t -> int
    val lookup : int -> t -> bblock
  end = struct

    module IntMap = Map.Make (Int)

    type t = {map: bblock IntMap.t; last: bblock; first: bblock}

    let start_block {first;_} = first

    let last_block {last;_} = last

    let build (instr: bblock list) =
      let get_start_line (bl: bblock) =  bl.start in
      let map =
        List.to_iter instr
        |> Iter.map (fun code -> (get_start_line code, code))
        |> IntMap.of_iter in
      let first = List.hd instr in
      let last = List.last_opt instr |> Option.get_exn in
      {map; last; first}

    let length {last;_} = last.start

    let lookup line tbl = 
      if line < length tbl then
        try
          IntMap.find line tbl.map
        with
          e -> print_endline @@ Printf.sprintf "could not find %d" line;
          raise e
      else tbl.last

  end

  let get_jump_target (bl: bblock) =  bl.nxt
  let get_next_line (bl: bblock) =  match bl.nxt with
    | None -> bl.start + List.length bl.code
    | Some _ -> bl.start + 1 + List.length bl.code


  let build_graph (instr:bblock list) tbl =
    let graph = G.create () in
    let add_edges block =
      match (get_jump_target block) with
      | None ->
        let target = BlockTable.lookup (get_next_line block) tbl in
        G.add_edge graph block target
      | Some target ->
        let target = BlockTable.lookup target tbl in
        G.add_edge graph block target in
    List.iter (G.add_vertex graph) instr;
    List.iter add_edges instr;
    graph

  (** returns a set of all blocks that can reach the given block *)
  let get_reachable (graph: G.t) (block: bblock) : BlockSet.t =
    let rec loop set block =
      if BlockSet.mem block set
      then set
      else begin
        let set = BlockSet.add block set in
        G.pred graph block
        |> List.to_iter
        |> Iter.fold loop set
      end
    in
    loop BlockSet.empty block


  let run_analysis (instr: bblock list) =
    let tbl = BlockTable.build instr in
    let graph = build_graph instr tbl in
    (* check_can_hit_reachable [block] [set] = Some [line_no] if
       changing the instruction at absolute [line_no] of the [block]
       causes the execution to hit a block in [set] *)
    let check_can_hit_reachable (block: bblock) (set: BlockSet.t) : int option =
      assert (not (BlockSet.mem block set));
      let (let+) x f = Option.bind x f in
      (* try changing jump to nop *)
      let try_jump_target =
        (* if block ends with a jump to some arbitrary jump *)
        let+ _ = block.nxt in
        (* then if the jump were elided, the code would move to the next block  *)
        let next_block = BlockTable.lookup (get_next_line block) tbl in
        (* so if the next block is in the set *)
        let+ _ = Option.if_ (Fun.flip BlockSet.mem set) next_block in
        (* then we just need to change the jump instruction *)
        Some (block.start + List.length block.code) in
      (* try changing nops to jumps *)
      let try_nop_target () =
        let find_instr_target block = function
          | (_, BAcc _) -> None
          | (instr_offset, BNop relative) ->
            Some (
              block.start + instr_offset,
              BlockTable.lookup (block.start + instr_offset + relative) tbl
            ) in
        block.code
        |> Iter.of_list
        |> Iter.zip_i
        |> Iter.filter_map (find_instr_target block)
        |> Iter.fold_while (fun _ (line_no, bl) ->
            if BlockSet.mem bl set
            then Some line_no, `Stop
            else None, `Continue
          ) None in
      Option.or_lazy
        try_jump_target
        ~else_:try_nop_target in
    (* searches using dfs through the block graph for a line to change to hit the target  *)
    let search_for_change_to_hit_set (block: bblock) set =
      let seen_set = ref BlockSet.empty in
      let has_seen_block block =
        if BlockSet.mem block !seen_set
        then true
        else begin
          seen_set := BlockSet.add block !seen_set;
          false
        end in
      let rec loop block =
        if has_seen_block block
        then None
        else
          match check_can_hit_reachable block set with
          | Some s -> Some s
          | None ->
            G.succ graph block
            |> Iter.of_list
            |> Iter.fold_while (fun _ successor_block ->
                match loop successor_block with
                | None -> None, `Continue
                | Some s -> Some s, `Stop
              ) None in
      loop block in
    let terminating_set = get_reachable graph (BlockTable.last_block tbl) in
    let line_change = search_for_change_to_hit_set (BlockTable.start_block tbl) terminating_set in
    Option.get_exn line_change

  let flip_code code line_no =
    match code.(line_no) with
    | Jmp v -> code.(line_no) <- Nop v
    | Nop v -> code.(line_no) <- Jmp v
    | Acc _ -> failwith "invalid analysis"


  let run_code_to_completion code =
    let exception Completed of int in
    let check_state (exec_state: exec_state) =
      if exec_state.instr >= Array.length code
      then raise (Completed exec_state.acc)
      else () in
    let rec loop exec_state =
      let exec_state = step code exec_state in
      check_state exec_state;
      loop exec_state in
    try
      ignore @@ loop initial_exec_state;
      failwith "never terminates"
    with
    | Completed vl -> vl

  let result =
    let bblocks = extract_basic_blocks code in
    let line_to_change = run_analysis bblocks in
    flip_code code line_to_change;
    run_code_to_completion code


  let () = print_int result

end

