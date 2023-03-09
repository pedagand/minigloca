open MiniglocaLib
open MiniglocaLib.Meta

let deadcode_elimination_test_gloca tag ast check =
  let ddc = deadcode_elimination ast in
  Alcotest.(check (option (Alcotest.testable Ast.pp_s Ast.equal_s)))
    tag check ddc

let deadcode_elimination_test () =
  deadcode_elimination_test_gloca "Deadcode elimination test #1"
    (Test_1.gloca ())
    (Some
       Ast.Syntax.(
         ("a" := Int 5) ^ whiledo (Id "a" < Int 50) ("a" := Id "a" + Int 1)));

  deadcode_elimination_test_gloca "Deadcode elimination test #2"
    (Test_2.gloca ())
    (Some
       Ast.Syntax.(
         ("b" := Int 20) ^ whiledo (Id "b" < Int 100) ("b" := Id "b" + Int 1)));

  deadcode_elimination_test_gloca "Deadcode elimination test #3"
    (Test_3.gloca ())
    (Some (Test_3.gloca ()))
