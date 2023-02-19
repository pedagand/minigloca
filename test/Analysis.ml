open MiniglocaLib.Analysis
open MiniglocaLib.Label

let pp_map_iter f m = LabelMap.iter (fun _ v -> f v) m

let test_gloca_dataflow tag ast check_lin check_lout =
  let lin, lout = dataflow ast in
  let lm_testable =
    Alcotest.testable
      (Fmt.brackets (Fmt.iter ~sep:(Fmt.any "; ") pp_map_iter (Fmt.braces (Fmt.iter ~sep:(Fmt.any ", ") Vars.iter Fmt.string))))
      (LabelMap.equal Vars.equal)
  in
  Alcotest.(check lm_testable) tag check_lin lin;
  Alcotest.(check lm_testable) tag check_lout lout

let test_dataflow () =
  let top = Vars.of_list [ "a"; "b" ] in
  let a = Vars.of_list [ "a" ] in
  let b = Vars.of_list [ "b" ] in
  let bot = Vars.empty in
  let ltomap m (k, v) = LabelMap.add k v m in

  test_gloca_dataflow "Test DF #1" (Test_1.gloca ())
    (List.fold_left ltomap LabelMap.empty
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
       ]);

  test_gloca_dataflow "Test DF #2" (Test_2.gloca ())
    (List.fold_left ltomap LabelMap.empty
       [ (1, b); (2, b); (3, b); (4, b); (5, top); (6, a); (7, bot) ])
    (List.fold_left ltomap LabelMap.empty
       [ (1, b); (2, b); (3, b); (4, b); (5, b); (6, top); (7, a) ]);

  test_gloca_dataflow "Test DF #3" (Test_3.gloca ())
    (List.fold_left ltomap LabelMap.empty
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
       ])
