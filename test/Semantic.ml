open MiniglocaLib

let test_eval_stm () =
  let final =
    List.fold_left
      (fun s (k, v) -> Interpreter.State.add k v s)
      Interpreter.State.empty
      [ ("a", 0); ("b", 8); ("c", 1); ("f", 2) ]
  in
  Alcotest.(check bool)
    "Statement evaluation" true
    (final = Interpreter.eval_stm (Test_1.gloca ()) Interpreter.State.empty)

let test_iswf () =
  Alcotest.(check bool)
    "Is program well formed" true
    (Label.is_statement_well_formed (Test_1.gloca ()))
