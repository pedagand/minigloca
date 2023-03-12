let () =
  Random.self_init ();
  let open Alcotest in
  run "Minigloca"
    [
      ( "syntax-tests",
        [ test_case "Tests over Syntax module" `Quick Syntax.test_syntax ] );
      ( "semantic-tests",
        [
          test_case "Tests over interpreter" `Quick Interpreter.test_interpreter;
          test_case "Tests over well formed statements" `Quick
            Interpreter.test_iswf;
        ] );
      ( "cfg-tests",
        [
          test_case "Tests over Control Flow Graph construction" `Quick
            ControlFlow.test_cfg;
        ] );
      ( "analysis-tests",
        [
          test_case "Tests over dataflow" `Slow Analysis.test_dataflow;
          test_case "Tests over deadcode elimination" `Quick
            Meta.deadcode_elimination_test;
        ] );
    ]
