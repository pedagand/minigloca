open MiniglocaLib

let gloca () =
  Ast.fl := 0;
  Ast.Syntax.(
    ("a" := Int 1)
    ^ ("b" := Int 0)
    ^ whiledo (Id "b" < Int 100)
        (ifte (Id "a" < Int 50) ("a" := Id "a" * Int 2) ("a" := Int 100)
        ^ whiledo (Id "a" < Id "b") ("a" := Id "a" + Int 1)
        ^ ("b" := Id "b" + Int 1)))
