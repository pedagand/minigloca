open MiniglocaLib

let test_syntax_plus () =
        let (a, b) = Random.int 1000, Random.int 1000 in
        Alcotest.(check bool) "Same arithmetic expression" 
        true 
        (Ast.equal_a (Plus(Int(a), Int(b))) Ast.Syntax.(Int(a) + Int(b)))

let test_syntax_minus () =
        let (a, b) = Random.int 1000, Random.int 1000 in
        Alcotest.(check bool) "Same arithmetic expression" 
        true 
        (Ast.equal_a (Minus(Int(a), Int(b))) Ast.Syntax.(Int(a) - Int(b)))

let test_syntax_times () =
        let (a, b) = Random.int 1000, Random.int 1000 in
        Alcotest.(check bool) "Same arithmetic expression" 
        true 
        (Ast.equal_a (Times(Int(a), Int(b))) Ast.Syntax.(Int(a) * Int(b)))

let () =
        Random.self_init(); 
        let open Alcotest in
        run "Minigloca" [
                "syntax-tests", [
                        test_case "Test syntax Plus" `Quick test_syntax_plus;
                        test_case "Test syntax Minus" `Quick test_syntax_minus;
                        test_case "Test syntax Times" `Quick test_syntax_times;
                        test_case "Gloca Program Test #1" `Quick Test_1.test;
                ]
                ]
                
