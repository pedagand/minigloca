open MiniglocaLib

let gloca () = Ast.Syntax.(
        ("a" := Int(5))^
        ("b" := Int(8))^

        whiledo (Id("a") < Int(50))
        (
                ("c" := Int(1))^
                ("f" := Int(2))^
                ("a" := Id("a") + Int(1))
        )^

        ifte (Id("b") < Int(2))
        (
                "a" := Int(1)
        )
        (
                "a" := Int(0)
        )
)

let test () =
        Alcotest.(check bool) "Is program well formed"
        true
        (Label.isStatementWellFormed (gloca()))
