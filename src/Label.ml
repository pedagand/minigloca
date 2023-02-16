open Ast

type block = BlAssign of string * a | BlExpBool of b | BlSkip
[@@deriving show]

module Label = struct
  type t = int

  let compare l_a l_b = Stdlib.compare l_a l_b
end

module Edge = struct
  type t = int * int [@@deriving ord]

  let compare e_a e_b = compare e_a e_b
end

module LabelMap = Map.Make (Label)
module LabelSet = Set.Make (Label)

let rec blocks_of stm blocks =
  match stm with
  | Assign t ->
      let s, a = t.cnt in
      LabelMap.add t.label (BlAssign (s, a)) blocks
  | Seq (s_1, s_2) ->
      let bl_1 = blocks_of s_1 blocks in
      blocks_of s_2 bl_1
  | Skip t -> LabelMap.add t.label BlSkip blocks
  | Ifte (t, s_1, s_2) ->
      let bl_b = LabelMap.add t.label (BlExpBool t.cnt) blocks in
      let bl_1 = blocks_of s_1 bl_b in
      blocks_of s_2 bl_1
  | While (t, s) ->
      let bl_b = LabelMap.add t.label (BlExpBool t.cnt) blocks in
      blocks_of s bl_b

let labels stm =
  let rec acc lb_list = function
    | Assign t -> t.label :: lb_list
    | Skip t -> t.label :: lb_list
    | Ifte (t, s_1, s_2) -> t.label :: acc (acc lb_list s_1) s_2
    | While (t, s) -> t.label :: acc lb_list s
    | Seq (s_1, s_2) -> acc (acc lb_list s_1) s_2
  in
  acc [] stm

let isStatementWellFormed stm =
  let lbls = labels stm in
  (* On ne considèrera pas la liste triée *)
  let lbls_sorted = List.sort compare_label lbls in
  let rec check = function
    | [] -> true
    | x :: [] -> true
    | x :: y :: tl -> if compare_label x y = 0 then false else check (y :: tl)
  in
  check lbls_sorted

let rec init stm =
  match stm with
  | Assign t -> t.label
  | Skip t -> t.label
  | Ifte (t, _, _) -> t.label
  | While (t, _) -> t.label
  | Seq (s_1, _) -> init s_1

let rec final stm =
  match stm with
  | Assign t -> LabelSet.singleton t.label
  | Skip t -> LabelSet.singleton t.label
  | While (t, _) -> LabelSet.singleton t.label
  | Seq (_, s_2) -> final s_2
  | Ifte (_, s_1, s_2) -> LabelSet.union (final s_1) (final s_2)

module EdgeSet = Set.Make (Edge)

let cartesian li_a li_b =
  List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) li_b) li_a)

let rec flow stm edges =
  match stm with
  | Assign _ | Skip _ -> EdgeSet.empty
  | Seq (s_1, s_2) ->
      let fl_1 = flow s_1 edges in
      let fl_2 = flow s_2 fl_1 in
      EdgeSet.union fl_2
        (EdgeSet.of_list
           (cartesian (LabelSet.elements (final s_1)) [ init s_2 ]))
  | Ifte (t, s_1, s_2) ->
      let fl_1 = flow s_1 edges in
      let fl_2 = flow s_2 edges in
      let ls_1 = EdgeSet.add (t.label, init s_1) edges in
      EdgeSet.add (t.label, init s_2) ls_1
  | While (t, s) ->
      let fl_s = flow s edges in
      let ls_s = EdgeSet.add (t.label, init s) fl_s in
      EdgeSet.union ls_s
        (EdgeSet.of_list (cartesian (LabelSet.elements (final s)) [ t.label ]))
