open MiniglocaLib

let test_syntax_gloca tag ast check =
  Alcotest.(check (Alcotest.testable Ast.pp_s Ast.equal_s)) tag check ast

let test_syntax () =
  test_syntax_gloca "Syntax #1" (Test_1.gloca ())
    (Ast.Seq
       ( Ast.Assign { Ast.cnt = ("a", Ast.Int 5); label = 9 },
         Ast.Seq
           ( Ast.Assign { Ast.cnt = ("b", Ast.Int 8); label = 8 },
             Ast.Seq
               ( Ast.While
                   ( { Ast.cnt = Ast.Lt (Ast.Id "a", Ast.Int 50); label = 7 },
                     Ast.Seq
                       ( Ast.Assign { Ast.cnt = ("c", Ast.Int 1); label = 6 },
                         Ast.Seq
                           ( Ast.Assign { Ast.cnt = ("f", Ast.Int 2); label = 5 },
                             Ast.Assign
                               {
                                 Ast.cnt =
                                   ("a", Ast.Plus (Ast.Id "a", Ast.Int 1));
                                 label = 4;
                               } ) ) ),
                 Ast.Ifte
                   ( { Ast.cnt = Ast.Lt (Ast.Id "b", Ast.Int 2); label = 3 },
                     Ast.Assign { Ast.cnt = ("a", Ast.Int 1); label = 2 },
                     Ast.Assign { Ast.cnt = ("a", Ast.Int 0); label = 1 } ) ) )
       ));

  test_syntax_gloca "Syntax #2" (Test_2.gloca ())
    (Ast.Seq
       ( Ast.Assign { Ast.cnt = ("a", Ast.Int 1); label = 7 },
         Ast.Seq
           ( Ast.Assign { Ast.cnt = ("b", Ast.Int 20); label = 6 },
             Ast.Seq
               ( Ast.Ifte
                   ( { Ast.cnt = Ast.Lt (Ast.Id "a", Ast.Int 3); label = 5 },
                     Ast.Assign { Ast.cnt = ("c", Ast.Int 4); label = 4 },
                     Ast.Assign { Ast.cnt = ("c", Ast.Int 6); label = 3 } ),
                 Ast.While
                   ( { Ast.cnt = Ast.Lt (Ast.Id "b", Ast.Int 100); label = 2 },
                     Ast.Assign
                       {
                         Ast.cnt = ("b", Ast.Plus (Ast.Id "b", Ast.Int 1));
                         label = 1;
                       } ) ) ) ));

  test_syntax_gloca "Syntax #3" (Test_3.gloca ())
    (Ast.Seq
       ( Ast.Assign { Ast.cnt = ("a", Ast.Int 1); label = 9 },
         Ast.Seq
           ( Ast.Assign { Ast.cnt = ("b", Ast.Int 0); label = 8 },
             Ast.While
               ( { Ast.cnt = Ast.Lt (Ast.Id "b", Ast.Int 100); label = 7 },
                 Ast.Seq
                   ( Ast.Ifte
                       ( { Ast.cnt = Ast.Lt (Ast.Id "a", Ast.Int 50); label = 6 },
                         Ast.Assign
                           {
                             Ast.cnt = ("a", Ast.Times (Ast.Id "a", Ast.Int 2));
                             label = 5;
                           },
                         Ast.Assign { Ast.cnt = ("a", Ast.Int 100); label = 4 }
                       ),
                     Ast.Seq
                       ( Ast.While
                           ( {
                               Ast.cnt = Ast.Lt (Ast.Id "a", Ast.Id "b");
                               label = 3;
                             },
                             Ast.Assign
                               {
                                 Ast.cnt =
                                   ("a", Ast.Plus (Ast.Id "a", Ast.Int 1));
                                 label = 2;
                               } ),
                         Ast.Assign
                           {
                             Ast.cnt = ("b", Ast.Plus (Ast.Id "b", Ast.Int 1));
                             label = 1;
                           } ) ) ) ) ))
