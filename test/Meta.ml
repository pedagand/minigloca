open MiniglocaLib
open MiniglocaLib.Meta

let deadcode_elimination_test_gloca tag ast =
  let ddc = deadcode_elimination ast in
  let check = incr_deadcode_elimination ast in
  Alcotest.(check (option (Alcotest.testable Ast.pp_s Ast.equal_s)))
    tag ddc check

let deadcode_elimination_test () =
  deadcode_elimination_test_gloca "Deadcode elimination test #1"
    (Test_1.gloca ());

  deadcode_elimination_test_gloca "Deadcode elimination test #2"
    (Test_2.gloca ());
    
  deadcode_elimination_test_gloca "Deadcode elimination test #3"
    (Test_3.gloca ())