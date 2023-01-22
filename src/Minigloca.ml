open Ast

let fl = ref 0
let fetch () =  
        fl := !fl + 1;
        !fl

module Syntax = struct 

        let (+) a_1 a_2 = Plus(a_1, a_2)
        let (-) a_1 a_2 = Minus(a_1, a_2)
        let ( * ) a_1 a_2 = Times(a_1, a_2)

        let (:=) ?(l = fetch ()) x a = Assign(x, a, l)
        let (^) s_1 s_2 = Seq(s_1, s_2)
        let ifte ?(l = fetch ()) b s_1 s_2 = Ifte(b, s_1, s_2, l)
        let whiledo ?(l = fetch ()) b s = While(b, s, l)
        let skip () = Skip, None
end

let main () =
        Printf.printf "Launching Minigloca...\n"; 
        let tree = Syntax.(
                ("x" := Int(5)) 
                ^ (ifte True ("z" := Id("x")) ("z" := Int(1)))
                ^ (whiledo True ("z" := Id("x") + Int(1)))
        ) in
        let u = Plus(Id("x"), Int(4)) in
        let t = Plus(Id("x"), Int(5)) in 
        Printf.printf "%s\n%s\n%b\n" (show_s tree) (aexpToString t) (equal_a t u);
        exit 0

let _ = if not !Sys.interactive then main () else ()
