open MiniglocaLib.Ast
open MiniglocaLib.Meta

let deadcode_elimination_test_gloca tag ast =
  let nv_reduction = deadcode_elimination ast in
  let ic_reduction = incr_deadcode_elimination ast in
  Alcotest.(check ((Alcotest.testable pp_s equal_s)))
    tag nv_reduction ic_reduction

let deadcode_elimination_test () = Generator.iterate_on 2 deadcode_elimination_test_gloca 0 10