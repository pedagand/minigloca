open MiniglocaLib

let gloca () =
  Ast.fl := 0;
  Ast.Syntax.(
    ("a" := Int 1)
    ^ ("b" := Int 20)
    ^ ifte (Id "a" < Int 3) ("c" := Int 4) ("c" := Int 6)
    ^ whiledo (Id "b" < Int 100) ("b" := Id "b" + Int 1))
