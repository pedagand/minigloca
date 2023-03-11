open Ast
open Random

let rec gen_a vars i depth =
  let depth_index = if depth = 0 then 2 else 3 in
  let bound = if i = 0 then 1 else depth_index in
  let nd = depth - 1 in
  let op = Random.int bound in
  match op with
  | 0 -> Int (Random.bits ())
  | 1 -> Id vars.(Random.int i)
  | 2 -> Plus (gen_a vars i nd, gen_a vars i nd)
  | 3 -> Minus (gen_a vars i nd, gen_a vars i nd)
  | _ -> Times (gen_a vars i nd, gen_a vars i nd)

let rec gen_b vars i depth =
  let bound = 7 in
  let op = Random.int bound in
  match op with
  | 0 -> True
  | 1 -> False
  | 2 -> Lt (gen_a vars i depth, gen_a vars i depth)
  | 3 -> Eq (gen_a vars i depth, gen_a vars i depth)
  | 4 -> And (gen_b vars i 1, gen_b vars i 1)
  | 5 -> Or (gen_b vars i 1, gen_b vars i 1)
  | _ -> Not (gen_b vars i 1)

let gen_nomenclature x = string_of_int x

let rec generate vars i bound =
  if i = Array.length vars || i >= bound then Syntax.skip ()
  else
    let ni, s_1 = gen_s vars i in
    Syntax.( ^ ) s_1 (generate vars ni bound)

and gen_s vars i =
  let len = Array.length vars in
  let gamma = 3 * len / 5 * len in
  let bound = 3 in
  let op = max 0 (Random.int (gamma * bound) - (gamma - 1) * bound) in
  match op with
  | 0 -> (i + 1, gen_assign vars i)
  | 1 -> gen_ifte vars i
  | _ -> gen_while vars i

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
  (i + sub_bound, Syntax.whiledo (gen_b vars i 1) (generate vars i (i + sub_bound)))