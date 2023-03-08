open MiniglocaLib.Interpreter
open MiniglocaLib.Label

let pp_state_map_iter f m = State.iter (fun k v -> f (k, v)) m

let test_interpreter_gloca tag ast check =
  let inp_testable =
    Alcotest.testable
      (Fmt.brackets
         (Fmt.iter ~sep:(Fmt.any "; ") pp_state_map_iter
            (Fmt.parens (Fmt.pair ~sep:(Fmt.any ", ") Fmt.string Fmt.int))))
      (State.equal ( = ))
  in
  Alcotest.(check inp_testable) tag check (eval_stm ast State.empty)

let test_interpreter () =
  let folder li =
    List.fold_left (fun s (k, v) -> State.add k v s) State.empty li
  in
  test_interpreter_gloca "Test Interpreter #1" (Test_1.gloca ())
    (folder [ ("a", 0); ("b", 8); ("c", 1); ("f", 2) ]);
  test_interpreter_gloca "Test Interpreter #2" (Test_2.gloca ())
    (folder [ ("a", 1); ("b", 100); ("c", 4) ]);
  test_interpreter_gloca "Test Interpreter #3" (Test_3.gloca ())
    (folder [ ("a", 100); ("b", 100) ])

let test_iswf_gloca tag ast =
  Alcotest.(check bool) tag true (is_statement_well_formed ast)

let test_iswf () =
  test_iswf_gloca "ISWF #1" (Test_1.gloca ());
  test_iswf_gloca "ISWF #2" (Test_2.gloca ());
  test_iswf_gloca "ISWF #3" (Test_3.gloca ())
