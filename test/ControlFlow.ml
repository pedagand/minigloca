open MiniglocaLib

let test_gloca_cfg tag ast check =
  let cfg = Label.EdgeSet.of_list check in
  let test_p_cfg = Label.flow_of ast in
  Alcotest.(check bool) tag true (Label.EdgeSet.equal cfg test_p_cfg)

let test_cfg () =
  test_gloca_cfg "CFG Test #1" (Test_1.gloca ())
    [ (3, 1); (3, 2); (4, 7); (5, 4); (6, 5); (7, 3); (7, 6); (8, 7); (9, 8) ];

  test_gloca_cfg "CFG Test #2" (Test_2.gloca ())
    [ (1, 2); (2, 1); (3, 2); (4, 2); (5, 3); (5, 4); (6, 5); (7, 6) ];

  test_gloca_cfg "CFG Test #3" (Test_3.gloca ())
    [ (1, 7); (2, 3); (3, 1); (3, 2); (4, 3); (5, 3); (6, 4); (6, 5); (7, 6); (8, 7); (9, 8) ]
