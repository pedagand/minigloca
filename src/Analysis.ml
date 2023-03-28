open Ast
open Label
module Vars = Set.Make (String)

let rec vars_a e =
  match e with
  | Int i -> Vars.empty
  | Id s -> Vars.singleton s
  | Plus (a_1, a_2) | Minus (a_1, a_2) | Times (a_1, a_2) ->
      Vars.union (vars_a a_1) (vars_a a_2)

let rec vars_b e =
  match e with
  | True | False -> Vars.empty
  | Lt (a_1, a_2) | Eq (a_1, a_2) -> Vars.union (vars_a a_1) (vars_a a_2)
  | And (b_1, b_2) | Or (b_1, b_2) -> Vars.union (vars_b b_1) (vars_b b_2)
  | Not b -> vars_b b

let gen block =
  match block with
  | BlAssign (_, a) -> vars_a a
  | BlExpBool b -> vars_b b
  | BlSkip -> Vars.empty

let kill block =
  match block with
  | BlAssign (s, _) -> Vars.singleton s
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
  | (l, b) :: t ->
      let live_out = successor_blocks_union (succ an_s.flow l) lin in
      let live_in = Vars.union (gen b) (Vars.diff live_out (kill b)) in
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
      let live_in = Vars.union (gen b) (Vars.diff live_out (kill b)) in
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

let pprint_vars vars = Vars.iter (fun e -> Printf.printf "%s, " e) vars

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

(*
  There we suppose that the given program is
  included in the program that produced the fp
  analysis.

  It takes the program P before reduction
  the label l of the reduced block and the
  dataflow analysis of the program P.

  This always returns a pre fixed-point (see the proof)
  In fact, it can be noted that we always reach a fixed point.
*)

let dataflow_filter_bloc p l fp =
  let an_s = build_analysis_structure p in
  let bloc_gen = gen (LabelMap.find l an_s.blocks) in
  let rec go edges analysis =
    match edges with
    | [] -> analysis
    | (e, e') :: t when e' = l ->
        let lin, lout = analysis in
        (*
        In this case LIVE_OUT is always altered   
        *)
        let bloc_out_set = LabelMap.find e lout in
        let reduced_bloc_set = Vars.diff bloc_gen bloc_out_set in
        let lout' = LabelMap.add e reduced_bloc_set lout in
        (*
        The LIVE_OUT case is different
        - if A belongs to gen[e] then we stop the graph search
        - otherwise LIVE_IN = LIVE_IN - A,  and it continues  
        *)
        let pred_gen = gen (LabelMap.find e an_s.blocks) in
        if Vars.subset bloc_gen pred_gen then (lin, lout')
        else
          let bloc_live_in = Vars.diff pred_gen bloc_gen in
          let lin' = LabelMap.add e bloc_live_in lin in
          go t (lin', lout')
    | h :: t -> analysis
  in
  let ffp, sfp = fp in
  go (EdgeSet.elements an_s.flow) (LabelMap.add l Vars.empty ffp, LabelMap.add l Vars.empty sfp)