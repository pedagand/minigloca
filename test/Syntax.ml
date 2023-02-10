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

let test_syntax_lt () =
        Alcotest.(check bool) "Lesser than operator"
        true
        (Ast.equal_b (Lt(Int(0), Int(1))) Ast.Syntax.(Int(0) < Int(1)))

let test_syntax_eq () =
        Alcotest.(check bool) "Equal operator"
        true
        (Ast.equal_b (Eq(Int(0), Int(1))) Ast.Syntax.(Int(0) = Int(1)))

let test_syntax_and () =
        Alcotest.(check bool) "And operator"
        true
        (Ast.equal_b (And(True, False)) Ast.Syntax.(True && False))

let test_syntax_or () =
        Alcotest.(check bool) "Or operator"
        true
        (Ast.equal_b (Or(True, False)) Ast.Syntax.(True || False))

let test_syntax_not () =
        Alcotest.(check bool) "Not operator"
        true
        (Ast.equal_b (Not(True)) Ast.Syntax.(not True))

let test_syntax_assign () =
        Ast.fl := !Ast.fl - !Ast.fl;
        Alcotest.(check bool) "Assign operator"
        true
        (Ast.equal_s (Ast.Assign{cnt = ("x", Ast.Int(1)); label = 5}) Ast.Syntax.("x" := Ast.Int(1)))
        