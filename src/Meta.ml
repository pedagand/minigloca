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

let rec iterative_reduction p stm analysis =
  let lin, lout = analysis in
  match stm with
  | Assign t as assign ->
      let id, _ = t.cnt in
      if Vars.mem (t.label, id) (LabelMap.find t.label lout) then
        (analysis, assign)
      else (dataflow_filter t.label analysis, Syntax.skip ~l:t.label ())
  (*
    Concerning the sequence, it is needed to take consideration of a parent
    program. In each part, we stabilize the pre-fixed point found.
  *)
  | Seq (s_1, s_2) when p = None ->
      let an_2, rs_2 = iterative_reduction (Some s_1) s_2 analysis in
      let p' = Seq (s_1, rs_2) in
      pprint_dataflow an_2;
      let an_1, rs_1 =
        iterative_reduction p s_1 an_2
      in
      (incr_dataflow (Seq (rs_1, rs_2)) an_1 dataflow_wl, Seq (rs_1, rs_2))
  (*
    The sequence in the case of a pre program.
  *)
  | Seq (s_1, s_2) ->
      let sp = Option.value p ~default:(Syntax.skip ()) in
      let an_2, rs_2 =
        iterative_reduction (Some (Seq (sp, s_1))) s_2 analysis
      in
      let p' = Seq (sp, Seq (s_1, rs_2)) in
      pprint_dataflow an_2;
      let an_1, rs_1 =
        iterative_reduction p s_1 an_2
      in
      ( incr_dataflow (Seq (sp, Seq (rs_1, rs_2))) an_1 dataflow_wl,
        Seq (rs_1, rs_2) )
  (*
    In case of a Skip statement, we just need to return it with
    the current analysis.     
  *)
  | Skip t as s -> (analysis, s)
  (*
    Concerning boolean blocks such as ifte and whiledo, we consider
    each of their statement to be an independent program, hence
    None parameter when calling iterative_reduction.

    It is useless to stabilize the pre-fixed point since at that point
    we don't know about the whole program, i.e. we don't have access to the
    whole control flow graph.
  *)
  | Ifte (t, s_1, s_2) ->
    Printf.printf "Reducing IFTE\n";
    let an_2, rs_2 = iterative_reduction None s_2 analysis in
    let an_1, rs_1 = iterative_reduction None s_1 an_2 in
      (an_1, Ifte (t, rs_1, rs_2))
  | While (t, s) as w ->
      let an, red = iterative_reduction p s analysis in
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
  (* let opt_reduced = skip_reduction reduced in
     match opt_reduced with
     | Some r when not (equal_s stm r) -> deadcode_elimination r
     | _ -> reduced *)
  if equal_s stm reduced then reduced else deadcode_elimination reduced

let incr_deadcode_elimination stm =
  let analysis = dataflow stm dataflow_nv in
  pprint_dataflow analysis;
  let _, reduced = iterative_reduction None stm analysis in
  (* skip_reduction reduced *)
  reduced
