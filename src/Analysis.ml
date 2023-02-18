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

(*
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

let rec successor_blocks_union succ of_set =
  match succ with
  | [] -> Vars.empty
  | h :: t ->
      Vars.union (LabelMap.find h of_set) (successor_blocks_union t of_set)

let rec update lblocks cfg lin lout =
  match lblocks with
  | [] -> (lin, lout)
  | (l, b) :: t ->
      let live_in =
        Vars.union (gen b) (Vars.diff (LabelMap.find l lout) (kill b))
      in
      let live_out = successor_blocks_union (succ cfg l) lin in
      update t cfg (LabelMap.add l live_in lin) (LabelMap.add l live_out lout)

let rec dataflow_nv lblocks cfg lin lout =
  let lin', lout' = update (LabelMap.bindings lblocks) cfg lin lout in
  if LabelMap.equal Vars.equal lin lin' && LabelMap.equal Vars.equal lout lout'
  then (lin', lout')
  else dataflow_nv lblocks cfg lin' lout'

let rec dataflow_wl ?(wl=[]) lblocks cfg lin lout =
  match wl with
  | [] -> (lin, lout)
  | l :: t ->
      let b = LabelMap.find l lblocks in
      let live_out' = LabelMap.find l lout in
      let live_in' = LabelMap.find l lin in
      let live_in = Vars.union (gen b) (Vars.diff live_out' (kill b)) in
      let succs = succ cfg l in
      let live_out = successor_blocks_union succs lin in
      let wl' = if live_out' = live_out then t else succs @ t in
      dataflow_wl ~wl:(wl') lblocks cfg
        (LabelMap.add l live_in lin)
        (LabelMap.add l live_out lout)

let dataflow wth stm =
  let labels = labels stm in
  let blocks = blocks_of stm LabelMap.empty in  
  let flow = flow_of stm in
  let fold_go m e = LabelMap.add e Vars.empty m in
  let lin = List.fold_left fold_go LabelMap.empty labels in
  let lout = List.fold_left fold_go LabelMap.empty labels in
  wth blocks flow lin lout