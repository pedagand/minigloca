open Ast
open Label
module Vars = Set.Make (String)

let rec vars_a e =
  match e with
  | Int i -> Vars.empty
  | Id s -> Vars.singleton s
  | Plus (a_1, a_2) | Minus (a_1, a_2) | Times (a_1, a_2) ->
      Vars.union (vars_a a_1) (vars_a a_2)

let rec vars_b e =
  match e with
  | True | False -> Vars.empty
  | Lt (a_1, a_2) | Eq (a_1, a_2) -> Vars.union (vars_a a_1) (vars_a a_2)
  | And (b_1, b_2) | Or (b_1, b_2) -> Vars.union (vars_b b_1) (vars_b b_2)
  | Not b -> vars_b b

let gen block =
  match block with
  | BlAssign (_, a) -> vars_a a
  | BlExpBool b -> vars_b b
  | BlSkip -> Vars.empty

let kill block =
  match block with
  | BlAssign (s, _) -> Vars.singleton s
  | BlExpBool _ | BlSkip -> Vars.empty
