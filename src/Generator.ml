open Ast
open Random

type a_actions =
  | ACTION_ID
  | ACTION_INT
  | ACTION_PLUS
  | ACTION_MINUS
  | ACTION_TIMES

let a_actions_list =
  [| ACTION_INT; ACTION_ID; ACTION_PLUS; ACTION_MINUS; ACTION_TIMES |]

type b_actions =
  | ACTION_TRUE
  | ACTION_FALSE
  | ACTION_LT
  | ACTION_EQ
  | ACTION_AND
  | ACTION_OR
  | ACTION_NOT

let b_actions_list =
  [|
    ACTION_TRUE;
    ACTION_FALSE;
    ACTION_LT;
    ACTION_EQ;
    ACTION_AND;
    ACTION_OR;
    ACTION_NOT;
  |]

type s_actions = ACTION_ASSIGN | ACTION_IFTE | ACTION_WHILE

let s_actions_list = [| ACTION_ASSIGN; ACTION_IFTE; ACTION_WHILE |]

let rec gen_a vars i depth =
  let depth_index = if depth = 0 then 2 else Array.length a_actions_list in
  let bound = if i = 0 then 1 else depth_index in
  let nd = depth - 1 in
  let op = a_actions_list.(Random.int bound) in
  match op with
  | ACTION_INT -> Int (Random.bits ())
  | ACTION_ID -> Id vars.(Random.int i)
  | ACTION_PLUS -> Plus (gen_a vars i nd, gen_a vars i nd)
  | ACTION_MINUS -> Minus (gen_a vars i nd, gen_a vars i nd)
  | ACTION_TIMES -> Times (gen_a vars i nd, gen_a vars i nd)

let rec gen_b vars i depth =
  let bound = Array.length b_actions_list in
  let op = b_actions_list.(Random.int bound) in
  match op with
  | ACTION_TRUE -> True
  | ACTION_FALSE -> False
  | ACTION_LT -> Lt (gen_a vars i depth, gen_a vars i depth)
  | ACTION_EQ -> Eq (gen_a vars i depth, gen_a vars i depth)
  | ACTION_AND -> And (gen_b vars i 1, gen_b vars i 1)
  | ACTION_OR -> Or (gen_b vars i 1, gen_b vars i 1)
  | ACTION_NOT -> Not (gen_b vars i 1)

let gen_nomenclature x = string_of_int x

let rec generate vars i bound =
  if i = Array.length vars || i >= bound then Syntax.skip ()
  else
    let ni, s_1 = gen_s vars i in
    Syntax.( ^ ) s_1 (generate vars ni bound)

and gen_s vars i =
  let gamma = Array.length vars / 2 + 1 in
  let bound = Array.length s_actions_list in
  let op = s_actions_list.(max 0 (Random.int (gamma * bound) - (gamma - 1) * bound)) in
  match op with
  | ACTION_ASSIGN -> (i + 1, gen_assign vars i)
  | ACTION_IFTE -> gen_ifte vars i
  | ACTION_WHILE -> gen_while vars i

and gen_assign vars i =
  let var_name = gen_nomenclature i in
  let ae = gen_a vars i 1 in
  vars.(i) <- var_name;
  Syntax.( := ) var_name ae

and gen_ifte vars i =
  let be = gen_b vars i 1 in
  let bound = Random.int (Array.length vars - i) in
  let bound_1 = bound / 2 in
  let s_1 = generate vars i (i + bound_1) in
  let s_2 = generate vars (i + bound_1) (i + bound) in
  (i + bound, Syntax.ifte be s_1 s_2)

and gen_while vars i =
  let sub_bound = Random.int (Array.length vars - i) in
  (i + sub_bound, Syntax.whiledo (gen_b vars i 2) (generate vars i (i + sub_bound)))