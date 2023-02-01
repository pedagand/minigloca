open Ast

type block =
        | BlAssign of string * a
        | BlExpBool of b
        | BlSkip
        [@@deriving show]

module Label =
        struct
                type t = int
                let compare l_a l_b = Stdlib.compare l_a l_b
        end

module Edge =
        struct
                type t = int * int [@@deriving ord]
                let compare e_a e_b = compare e_a e_b
        end

module LabelMap = Map.Make(Label)

let rec blocksOf stm blocks = match stm with
        | Assign(s, a, l) -> LabelMap.add l (BlAssign (s, a)) blocks 
        | Seq(s_1, s_2) -> 
                        let bl_1 = blocksOf s_1 blocks in
                        blocksOf s_2 bl_1
        | Skip(l) -> LabelMap.add l BlSkip blocks
        | Ifte(b, s_1, s_2, l) ->
                        let bl_b = LabelMap.add l (BlExpBool b) blocks in
                        let bl_1 = blocksOf s_1 bl_b in
                        blocksOf s_2 bl_1
        | While(b, s, l) ->
                        let bl_b = LabelMap.add l (BlExpBool b) blocks in
                        blocksOf s bl_b                

let labels stm =
        let rec acc lb_list = function
                | Assign(_, _, l) 
                | Skip(l) -> l :: lb_list
                | Ifte(_, s_1, s_2, l) -> l :: (acc (acc lb_list s_1) s_2)
                | While(_, s, l) -> l :: (acc lb_list s)
                | Seq(s_1, s_2) -> acc (acc lb_list s_1) s_2
        in
        acc [] stm 

let isStatementWellFormed stm =
        let lbls = labels stm in
        (* On ne considèrera pas la liste triée *)
        let lbls_sorted = List.sort compare_label lbls in
        let rec check = function
                | [] -> true
                | x :: [] -> true 
                | x :: y :: tl -> 
                                if (compare_label x y) = 0 then
                                        false
                                else
                                        check (y :: tl)
        in
        check lbls_sorted

let rec init stm = match stm with
        | Assign(_, _, l)
        | Skip(l)
        | Ifte(_, _, _, l)
        | While(_, _, l) -> l
        | Seq(s_1, _) -> init s_1

let rec final stm = match stm with
        | Assign(_, _, l)
        | Skip(l)
        | While(_, _, l) -> [l]
        | Seq(_, s_2) -> final s_2
        | Ifte(_, s_1, s_2, _) -> (final s_1) @ (final s_2)

module LSet (T : Set.OrderedType) =
        struct
                module S = Set.Make(T)
                let ofList l = List.fold_left (fun set e -> S.add e set) S.empty l
        end

module LabelPairSet = LSet(Edge)

let cartesian li_a li_b = List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) li_a) li_b) 

let rec flow stm edges = match stm with
        | Assign(_, _, _)
        | Skip(_) -> LabelPairSet.S.empty
        | Seq(s_1, s_2) -> 
                        let fl_1 = flow s_1 edges in
                        let fl_2 = flow s_2 fl_1 in
                        LabelPairSet.S.union fl_2 (LabelPairSet.ofList (cartesian [init s_2] (final s_1))) 
        | Ifte(b, s_1, s_2, l) ->
                        let fl_1 = flow s_1 edges in
                        let fl_2 = flow s_2 edges in
                        let ls_1 = LabelPairSet.S.add (l, init s_1) edges in
                        LabelPairSet.S.add (l, init s_2) ls_1
        | While(b, s, l) ->
                        let fl_s = flow s edges in
                        let ls_s = LabelPairSet.S.add (l, init s) fl_s in
                        LabelPairSet.S.union ls_s (LabelPairSet.ofList (cartesian [l] (final s)))
