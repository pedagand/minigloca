let () =
        Random.self_init(); 
        let open Alcotest in
        run "Minigloca" [
                "syntax-tests", [
                        test_case "Test syntax Plus" `Quick Syntax.test_syntax_plus;
                        test_case "Test syntax Minus" `Quick Syntax.test_syntax_minus;
                        test_case "Test syntax Times" `Quick Syntax.test_syntax_times;
                        test_case "Test syntax Lesser Than" `Quick Syntax.test_syntax_lt;
                        test_case "Test syntax Equal" `Quick Syntax.test_syntax_eq;
                        test_case "Test syntax And" `Quick Syntax.test_syntax_and;
                        test_case "Test syntax Or" `Quick Syntax.test_syntax_or;
                        test_case "Test syntax Not" `Quick Syntax.test_syntax_not;  
                ];
                "semantic-tests", [
                        test_case "Test Interpreter" `Quick Semantic.test_eval_stm; 
                        test_case "Gloca Semantic Formation" `Quick Semantic.test_iswf; 
                ];
                "cfg-tests", [
                        test_case "Control Flow Graph construction" `Quick ControlFlow.test_cfg;
                ]
         ]       
