open MiniglocaLib.Analysis
open MiniglocaLib.Label
(* open MiniglocaLib *)

let pp_map_iter f m = LabelMap.iter (fun _ v -> f v) m

let test_gloca_dataflow tag ast =
  let lin, lout = dataflow ast dataflow_wl in
  let lin', lout' = dataflow ast dataflow_nv in
  let lm_testable =
    Alcotest.testable
      (Fmt.brackets
         (Fmt.iter ~sep:(Fmt.any "; ") pp_map_iter
            (Fmt.braces (Fmt.iter ~sep:(Fmt.any ", ") Vars.iter Fmt.(pair ~sep:comma int string)))))
      (LabelMap.equal Vars.equal)
  in
  Alcotest.(check bool) "Stability" true (is_fixpoint_stable ast (lin, lout));
  Alcotest.(check lm_testable) tag lin' lin;
  Alcotest.(check lm_testable) tag lout' lout
  
let test_dataflow () = Generator.iterate_on 10 test_gloca_dataflow 0 20
(* let top = Vars.of_list [ "a"; "b" ] in
   let a = Vars.of_list [ "a" ] in
   let b = Vars.of_list [ "b" ] in
   let bot = Vars.empty in
   let ltomap m (k, v) = LabelMap.add k v m in *)

(* test_gloca_dataflow "Test DF #1" (Test_1.gloca ()); *)
(* (List.fold_left ltomap LabelMap.empty
      [
        (1, bot);
        (2, bot);
        (3, b);
        (4, top);
        (5, top);
        (6, top);
        (7, top);
        (8, a);
        (9, bot);
      ])
   (List.fold_left ltomap LabelMap.empty
      [
        (1, bot);
        (2, bot);
        (3, bot);
        (4, top);
        (5, top);
        (6, top);
        (7, top);
        (8, top);
        (9, a);
      ]); *)

(* test_gloca_dataflow "Test DF #2" (Test_2.gloca ()); *)
(* (List.fold_left ltomap LabelMap.empty
      [ (1, b); (2, b); (3, b); (4, b); (5, top); (6, a); (7, bot) ])
   (List.fold_left ltomap LabelMap.empty
      [ (1, b); (2, b); (3, b); (4, b); (5, b); (6, top); (7, a) ]); *)

(* test_gloca_dataflow "Test DF #3" (Test_3.gloca ()); *)
(* (List.fold_left ltomap LabelMap.empty
      [
        (1, top);
        (2, top);
        (3, top);
        (4, b);
        (5, top);
        (6, top);
        (7, top);
        (8, a);
        (9, bot);
      ])
   (List.fold_left ltomap LabelMap.empty
      [
        (1, top);
        (2, top);
        (3, top);
        (4, top);
        (5, top);
        (6, top);
        (7, top);
        (8, top);
        (9, a);
      ]) *)
