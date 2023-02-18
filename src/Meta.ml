open Ast
open Analysis
open Label

let rec reduction stm lv_analysis =
  match stm with
  | Assign t ->
      let id, _ = t.cnt in
      if Vars.mem id (LabelMap.find t.label lv_analysis) then Some (Assign t)
      else None
  | Seq (s_1, s_2) -> (
      let red_s_1 = reduction s_1 lv_analysis in
      let red_s_2 = reduction s_2 lv_analysis in
      match (red_s_1, red_s_2) with
      | Some rs_1, Some rs_2 -> Some (Seq (rs_1, rs_2))
      | Some _, None -> red_s_1
      | None, Some _ -> red_s_2
      | None, None -> None)
  | Skip t -> Some (Skip t)
  | Ifte (t, s_1, s_2) -> (
      let red_s_1 = reduction s_1 lv_analysis in
      let red_s_2 = reduction s_2 lv_analysis in
      match (red_s_1, red_s_2) with
      | Some rs_1, Some rs_2 -> Some (Ifte (t, rs_1, rs_2))
      | Some rs_1, None -> Some (Ifte (t, rs_1, Syntax.skip ()))
      | None, Some rs_2 -> Some (Ifte (t, Syntax.skip (), rs_2))
      | None, None -> None)
  | While (t, s) -> (
      let red = reduction s lv_analysis in
      match red with Some v -> Some (While (t, v)) | None -> None)

let rec deadcode_elimination stm =
  let _, lv_out = dataflow dataflow_nv stm in
  let reduced = reduction stm lv_out in
  match reduced with
  | Some r when not (equal_s stm r) -> deadcode_elimination r
  | _ -> reduced
