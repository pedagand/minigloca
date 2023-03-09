open MiniglocaLib.Analysis
open MiniglocaLib.Label

let pp_map_iter f m = LabelMap.iter (fun _ v -> f v) m

let test_gloca_dataflow tag ast =
  let lin, lout = dataflow ast DATAFLOW_WORKLIST in
  let lin', lout' = dataflow ast DATAFLOW_NAIVE in
  let lm_testable =
    Alcotest.testable
      (Fmt.brackets (Fmt.iter ~sep:(Fmt.any "; ") pp_map_iter (Fmt.braces (Fmt.iter ~sep:(Fmt.any ", ") Vars.iter Fmt.string))))
      (LabelMap.equal Vars.equal)
  in
  Alcotest.(check lm_testable) tag lin' lin;
  Alcotest.(check lm_testable) tag lout' lout

  let rec iterations vars_size i tests = 
    let vars = Array.make vars_size "" in
    let s = MiniglocaLib.Generator.generate vars 0 (Array.length vars) in
    test_gloca_dataflow (string_of_int i) s;
    
    if i < tests then iterations vars_size (i + 1) tests else () 

let test_dataflow () = iterations 100 0 10;
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
