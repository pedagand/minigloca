open Ast

module State = Map.Make(String)

let rec eval_aexp state exp =
        match exp with
        | Int(i) -> i
        | Id(s) -> State.find s state
        | Plus(a_1, a_2) -> (eval_aexp state a_1) + (eval_aexp state a_2)
        | Minus(a_1, a_2) -> (eval_aexp state a_1) - (eval_aexp state a_2)
        | Times(a_1, a_2) -> (eval_aexp state a_1) * (eval_aexp state a_2)

let rec eval_bexp state exp =
        match exp with
        | True -> true
        | False -> false
        | Lt(a_1, a_2) -> (eval_aexp state a_1) < (eval_aexp state a_2)
        | Eq(a_1, a_2) -> (eval_aexp state a_1) = (eval_aexp state a_2)
        | And(b_1, b_2) -> (eval_bexp state b_1) && (eval_bexp state b_2)
        | Or(b_1, b_2) -> (eval_bexp state b_1) || (eval_bexp state b_2)
        | Not b -> not (eval_bexp state b)

let rec eval_stm stm state =
        match stm with
        | Assign(s, a, _) -> State.add s (eval_aexp state a) state
        | Seq(s_1, s_2) -> eval_stm s_2 (eval_stm s_1 state)
        | Ifte(b, s_1, s_2, _) ->
                        let bexp = (eval_bexp state b) in
                        if bexp then
                               eval_stm s_1 state
                        else
                               eval_stm s_2 state
        | While(b, s, _) as w ->
                        let bexp = (eval_bexp state b) in
                        if bexp then
                                eval_stm w (eval_stm s state)
                        else
                                state 
        | Skip(_) -> state
