open MiniglocaLib

let test_cfg () =
        Ast.fl := 0;
        let ast = Test_1.gloca () in
        let cfg = Label.LabelPairSet.S.of_list 
        [(3, 1);(3, 2);(4, 7);(5, 4);(6, 5);(7, 3);(7, 6);(8, 7);(9, 8)] in
        let test_p_cfg = Label.flow ast Label.LabelPairSet.S.empty in
        
        Label.LabelPairSet.S.iter (fun (x, y) -> Printf.printf "%d, %d\n" x y) test_p_cfg;
        Alcotest.(check bool) "CFG test"
        true
        (Label.LabelPairSet.S.equal cfg test_p_cfg)
