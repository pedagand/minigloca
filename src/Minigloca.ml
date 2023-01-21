open Ast

module Syntax = struct
        let (:=) ?(l = 1) x a = Assign(x, a)
        let (^) s_1 s_2 = Seq(s_1, s_2)
        let ifte ?(l = 1) b s_1 s_2 = Ifte(b, s_1, s_2)
        let whiledo ?(l = 1) b s = While(b, s)
        let skip ?(l = 1) () = Skip
end

let main () =
        Printf.printf "Launching Minigloca...\n"; 
        let tree = Syntax.(
                ("x" := Int(5)) 
                ^ (ifte True ("z" := Id("x")) ("z" := Int(1)))
                ^ (whiledo True ("z" := Id("x") + Int(1)))
        ) in
        let t = Plus(Id("x"), Int(5)) in
        Printf.printf "%s\n%s\n" (show_s tree) (aexpToString t);
        exit 0

let _ = if not !Sys.interactive then main () else ()
