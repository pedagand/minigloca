open MiniglocaLib.Ast

let gloca () =
  fl := 0;
  Syntax.(
    ("a" := Int 1)
    ^ ("b" := Id "a")
    ^ ("c" := Id "a" + Id "b")
    ^ whiledo (Id "c" < Int 100)
        (ifte (Id "b" < Int 10)
           ("a" := Id "b" * Int 2)
           (whiledo
              (Id "a" * Id "b" < Int 60)
              (("d" := Int 5 + Id "a")
              ^ ("e" := Id "d" * Int 3)
              ^ ("b" := (Int 4 * Id "a") + Id "e")))
        ^ ("d" := (Int 3 * Id "c") + Id "b")
        ^ ifte (Int 100 < Id "c")
            ("c" := Id "d" + Int 1)
            ("c" := Id "c" + Int 1)
        ^ ("e" := Id "b")
        ^ ("b" := Id "b" + Int 3)))
