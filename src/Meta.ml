open Ast
open Analysis
open Label

let rec skip_reduction stm =
  match stm with
  | Assign t -> Some (Assign t)
  | Seq (s_1, s_2) -> (
      let opt_s_1 = skip_reduction s_1 in
      let opt_s_2 = skip_reduction s_2 in
      match (opt_s_1, opt_s_2) with
      | Some o_1, Some o_2 -> Some (Seq (o_1, o_2))
      | Some _, None -> opt_s_1
      | None, Some _ -> opt_s_2
      | None, None -> None)
  | Skip t as s -> None
  | Ifte (t, s_1, s_2) -> (
      let opt_s_1 = skip_reduction s_1 in
      let opt_s_2 = skip_reduction s_2 in
      match (opt_s_1, opt_s_2) with
      | Some o_1, Some o_2 -> Some (Ifte (t, o_1, o_2))
      | Some o_1, None -> Some (Ifte (t, o_1, s_1))
      | None, Some o_2 -> Some (Ifte (t, s_1, o_2))
      | None, None -> None)
  | While (t, s) -> (
      let opt_s = skip_reduction s in
      match opt_s with Some o_s -> Some (While (t, o_s)) | _ -> None)

let rec iterative_reduction prefix stm suffix analysis =
  let lin, lout = analysis in
  match stm with
  | Assign t as assign ->
      let id, _ = t.cnt in
      if Vars.mem (t.label, id) (LabelMap.find t.label lout) then
        (analysis, assign)
      else (dataflow_filter t.label analysis, Syntax.skip ~l:t.label ())
  (*
    Concerning the sequence, it is needed to take consideration of a prefix
    and a suffix program. In each part, we stabilize the pre-fixed point found.
  *)
  | Seq (s_1, s_2) ->
      let seq_prefix =
        match prefix with None -> Some s_1 | Some v -> Some (Seq (v, s_1))
      in
      let an_2, rs_2 = iterative_reduction seq_prefix s_2 suffix analysis in
      let seq_suffix =
        match suffix with None -> rs_2 | Some v -> Seq (rs_2, v)
      in
      pprint_dataflow an_2;
      let an_1, rs_1 = iterative_reduction prefix s_1 (Some seq_suffix) an_2 in
      let p' =
        match prefix with
        | None -> Seq (rs_1, seq_suffix)
        | Some v -> Seq (v, Seq (rs_1, seq_suffix))
      in
      (incr_dataflow p' an_1 dataflow_wl, Seq (rs_1, rs_2))
  (*
    In case of a Skip statement, we just need to return it with
    the current analysis.     
  *)
  | Skip t as s -> (analysis, s)
  (*
    Concerning boolean blocks such as ifte and whiledo, we consider
    each of their statement to be an independent program, hence
    None parameter when calling iterative_reduction.

    In the case of a while loop statement, it is necessary to
    add it as a prefix of the statement block when calling iterative_reduction.
  *)
  | Ifte (t, s_1, s_2) ->
      Printf.printf "Reducing IFTE\n";
      let an_2, rs_2 = iterative_reduction prefix s_2 suffix analysis in
      let an_1, rs_1 = iterative_reduction prefix s_1 suffix an_2 in
      (an_1, Ifte (t, rs_1, rs_2))
  | While (t, s) as w ->
      let while_prefix =
        match prefix with None -> Some w | Some v -> Some (Seq (v, w))
      in
      let an, red = iterative_reduction while_prefix s suffix analysis in
      (an, While (t, red))

let rec reduction stm lv_analysis =
  match stm with
  | Assign t ->
      let id, _ = t.cnt in
      if Vars.mem (t.label, id) (LabelMap.find t.label lv_analysis) then
        Assign t
      else Syntax.skip ~l:t.label ()
  | Seq (s_1, s_2) ->
      let red_s_1 = reduction s_1 lv_analysis in
      let red_s_2 = reduction s_2 lv_analysis in
      Seq (red_s_1, red_s_2)
  | Skip t as s -> s
  | Ifte (t, s_1, s_2) ->
      let red_s_1 = reduction s_1 lv_analysis in
      let red_s_2 = reduction s_2 lv_analysis in
      Ifte (t, red_s_1, red_s_2)
  | While (t, s) ->
      let red = reduction s lv_analysis in
      While (t, red)

let rec deadcode_elimination stm =
  let _, lv_out = dataflow stm dataflow_nv in
  let reduced = reduction stm lv_out in
  if equal_s stm reduced then reduced else deadcode_elimination reduced

let incr_deadcode_elimination stm =
  let analysis = dataflow stm dataflow_nv in
  pprint_dataflow analysis;
  let _, reduced = iterative_reduction None stm None analysis in
  reduced
