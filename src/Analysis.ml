open Ast
open Label
(* module Vars = Set.Make (String) *)

module LabelVarPair = struct
  type t = label * string

  let compare (_, s_1) (_, s_2) = String.compare s_1 s_2
end

module Vars = Set.Make (LabelVarPair)

let rec vars_a e from =
  match e with
  | Int i -> Vars.empty
  | Id s -> Vars.singleton (from, s)
  | Plus (a_1, a_2) | Minus (a_1, a_2) | Times (a_1, a_2) ->
      Vars.union (vars_a a_1 from) (vars_a a_2 from)

let rec vars_b e from =
  match e with
  | True | False -> Vars.empty
  | Lt (a_1, a_2) | Eq (a_1, a_2) -> Vars.union (vars_a a_1 from) (vars_a a_2 from)
  | And (b_1, b_2) | Or (b_1, b_2) -> Vars.union (vars_b b_1 from) (vars_b b_2 from)
  | Not b -> vars_b b from

let gen (l, block) =
  match block with
  | BlAssign (_, a) -> vars_a a l
  | BlExpBool b -> vars_b b l
  | BlSkip -> Vars.empty

let kill (l, block) =
  match block with
  | BlAssign (s, _) -> Vars.singleton (l, s)
  | BlExpBool _ | BlSkip -> Vars.empty

let succ cfg l =
  let rec acc edges s =
    match edges with
    | [] -> s
    | (e, e') :: t when e = l -> acc t (e' :: s)
    | _ :: t -> acc t s
  in
  acc (EdgeSet.elements cfg) []

let pred cfg l =
  let rec acc edges s =
    match edges with
    | [] -> s
    | (e, e') :: t when e' = l -> acc t (e :: s)
    | _ :: t -> acc t s
  in
  acc (EdgeSet.elements cfg) []

(*
  The final label is the label that has not
  any successors on the control flow graph.   
*)

let get_final_label cfg =
  let rec go rm_cfg =
    match rm_cfg with
    | [] -> 0
    | (_, e') :: t -> if succ cfg e' = [] then e' else go t
  in
  go (EdgeSet.elements cfg)

(*
  A naive idea of the algorithm:

  For all l in L
  LIVE_IN[l] := \emptyset
  LIVE_OUT[l] := \emptyset

  LIVE_IN' = LIVE_IN
  LIVE_OUT' = LIVE_OUT
  
  While LIVE_IN' = LIVE_IN && LIVE_OUT' = LIVE_OUT do
    For all l in L
      LIVE_IN[l] = GEN[l] + (LIVE_OUT[l] - KILL[l])
      LIVE_OUT[l] = UNION OF LIVE_IN[l'], l' in succ(l)
*)

type an_structure = {
  blocks : block LabelMap.t;
  lblocks : (label * block) list;
  flow : EdgeSet.t;
}

let rec successor_blocks_union succ of_set =
  match succ with
  | [] -> Vars.empty
  | h :: t ->
      Vars.union (LabelMap.find h of_set) (successor_blocks_union t of_set)

let rec update an_s lin lout =
  match an_s.lblocks with
  | [] -> (lin, lout)
  | (l, b) as k :: t ->
      let live_out = successor_blocks_union (succ an_s.flow l) lin in
      let live_in = Vars.union (gen k) (Vars.diff live_out (kill k)) in
      update { an_s with lblocks = t }
        (LabelMap.add l live_in lin)
        (LabelMap.add l live_out lout)

let rec dataflow_nv wl an_s lin lout =
  let lin', lout' = update an_s lin lout in
  if LabelMap.equal Vars.equal lin lin' && LabelMap.equal Vars.equal lout lout'
  then (lin', lout')
  else dataflow_nv wl an_s lin' lout'

(*
    Worklist algorithm implementation
*)

let rec dataflow_wl wl an_s lin lout =
  match wl with
  | [] -> (lin, lout)
  | l :: t ->
      let b = LabelMap.find l an_s.blocks in
      let live_out' = LabelMap.find l lout in
      let live_in' = LabelMap.find l lin in
      let succs = succ an_s.flow l in
      let live_out = successor_blocks_union succs lin in
      let live_in = Vars.union (gen (l, b)) (Vars.diff live_out (kill (l, b))) in
      let wl' = if live_in' = live_in then t else pred an_s.flow l @ t in
      dataflow_wl wl' an_s
        (LabelMap.add l live_in lin)
        (LabelMap.add l live_out lout)

let build_analysis_structure stm =
  let blocks = blocks_of stm LabelMap.empty in
  let flow = flow_of stm in
  { blocks; lblocks = LabelMap.bindings blocks; flow }

let dataflow stm algo =
  let labels = labels stm in
  let an_s = build_analysis_structure stm in
  let fold_go m e = LabelMap.add e Vars.empty m in
  let lin = List.fold_left fold_go LabelMap.empty labels in
  let lout = List.fold_left fold_go LabelMap.empty labels in
  algo labels an_s lin lout

(*
  Searchs a fixed point from a given dataflow analysis   
*)

let incr_dataflow stm (lin, lout) algo =
  let labels = labels stm in
  let an_s = build_analysis_structure stm in
  algo labels an_s lin lout

let is_fixpoint_stable stm fp =
  let lin, lout = fp in
  let an_s = build_analysis_structure stm in
  let lin', lout' = update an_s lin lout in
  LabelMap.equal Vars.equal lin lin' && LabelMap.equal Vars.equal lout lout'

let pprint_vars vars = Vars.iter (fun (l, b) -> Printf.printf "(%d:%s), " l b) vars

let pprint_dataflow (lin, lout) =
  LabelMap.iter
    (fun key e ->
      Printf.printf "%d -> {" key;
      pprint_vars e;
      Printf.printf "}\n")
    lin;
  Printf.printf "== OUT ==\n";
  LabelMap.iter
    (fun key e ->
      Printf.printf "%d -> {" key;
      pprint_vars e;
      Printf.printf "}\n")
    lout

let dataflow_filter l fp =
  let din, dout = fp in
  let rec help rl vars =
    match rl with
    | [] -> vars
    | (lb, _) as k :: t when lb = l -> help t (Vars.remove k vars)
    | _ :: t -> help t vars 
  in
  let rec go at analysis =
    match at with
    | [] -> analysis
    | (k, v) :: t -> go t (LabelMap.add k (help (Vars.elements v) v) analysis)
  in
  go (LabelMap.bindings din) din, go (LabelMap.bindings dout) dout