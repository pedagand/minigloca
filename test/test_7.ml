open MiniglocaLib.Ast

let gloca () =
  fl := 0;
  Syntax.(
    ("a" := Int 1)
    ^ ("b" := Id "a")
    ^ ("c" := Id "a" + Id "b")
    ^ whiledo (Id "c" < Int 100)
        (ifte (Id "b" < Int 10) ("a" := Id "b" * Int 2) ("b" := Id "a" * Int 2)
        ^ ("d" := (Int 3 * Id "c") + Id "b")
        ^ ifte (Int 100 < Id "c")
            ("c" := Id "d" + Int 1)
            ("c" := Id "c" + Int 1)
        ^ ("e" := Id "b")
        ^ ("b" := Id "b" + Int 3))
    ^ ifte (Id "b" < Int 16)
        (("d" := Int 6) ^ ("a" := Id "d" * Int 6))
        ("d" := Id "c")
    ^ ("e" := Id "a"))
