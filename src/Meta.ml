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
      if Vars.mem id (LabelMap.find t.label lout) then (analysis, assign)
      else (dataflow_filter_bloc p t.label analysis, Syntax.skip ~l:t.label ())
  | Seq (s_1, s_2) ->
      let an_2, rs_2 = iterative_reduction p s_2 analysis in
      (* pprint_dataflow an_2; *)
      let p' = Seq (s_1, rs_2) in
      let an_1, rs_1 =
        iterative_reduction
          p
          s_1
          (incr_dataflow p' an_2 dataflow_wl)
      in
      (an_1, Seq (rs_1, rs_2))
  | Skip t as s -> (analysis, s)
  | Ifte (t, s_1, s_2) ->
      let an_1, rs_1 = iterative_reduction p s_1 analysis in
      let an_2, rs_2 = iterative_reduction p s_2 an_1 in
      (an_2, Ifte (t, rs_1, rs_2))
  | While (t, s) ->
      let an, red = iterative_reduction p s analysis in
      (an, While (t, red))

let rec reduction stm lv_analysis =
  match stm with
  | Assign t ->
      let id, _ = t.cnt in
      if Vars.mem id (LabelMap.find t.label lv_analysis) then Assign t
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
  (* pprint_dataflow analysis; *)
  let f_analysis, reduced = iterative_reduction stm stm analysis in
  (* skip_reduction reduced *)
  reduced
