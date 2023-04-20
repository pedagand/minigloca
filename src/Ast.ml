(* Arithmetic expressions *)

type a =
  | Int of int
  | Id of string
  | Plus of a * a
  | Minus of a * a
  | Times of a * a
[@@deriving show, eq]

let rec show_a_gloca a =
  match a with
  | Int i -> string_of_int i
  | Id s -> s
  | Plus (a_1, a_2) -> (show_a_gloca a_1) ^ " + " ^ (show_a_gloca a_2)
  | Minus (a_1, a_2) -> (show_a_gloca a_1) ^ " - " ^ (show_a_gloca a_2)
  | Times (a_1, a_2) -> (show_a_gloca a_1) ^ " * " ^ (show_a_gloca a_2)

(* Boolean expressions *)

type b =
  | True
  | False
  | Lt of a * a
  | Eq of a * a
  | And of b * b
  | Or of b * b
  | Not of b
[@@deriving show, eq]

let rec show_b_gloca b =
  match b with
  | True -> "true"
  | False -> "false"
  | Lt (a_1, a_2) -> (show_a_gloca a_1) ^ " < " ^ (show_a_gloca a_2)
  | Eq (a_1, a_2) -> (show_a_gloca a_1) ^ " = " ^ (show_a_gloca a_2)
  | And (b_1, b_2) -> (show_b_gloca b_1) ^ " & " ^ (show_b_gloca b_2)
  | Or (b_1, b_2) -> (show_b_gloca b_1) ^" | " ^ (show_b_gloca b_2)
  | Not b -> "not " ^ show_b_gloca b

(* Statements *)

type label = int [@@deriving show, eq, ord]

type 'a labelled = { cnt : 'a; label : label [@equal fun _ _ -> true] }
[@@deriving show, eq]

type s =
  | Assign of (string * a) labelled
  | Seq of s * s
  | Ifte of b labelled * s * s
  | While of b labelled * s
  | Skip of unit labelled
[@@deriving show, eq]

let rec show_s_gloca stm =
  match stm with
  | Assign l -> let s, v = l.cnt in s ^ " := " ^ (show_a_gloca v)
  | Seq(s_1, s_2) -> (show_s_gloca s_1) ^ ";\n" ^ (show_s_gloca s_2)
  | Ifte(l, s_1, s_2) -> "if " ^ (show_b_gloca l.cnt) ^ " then\n" ^ (show_s_gloca s_1) ^ "\nelse\n" ^ (show_s_gloca s_2) ^ "\nendif"
  | While(l, s) -> "while " ^ (show_b_gloca l.cnt) ^ " do\n" ^ (show_s_gloca s) ^ "\ndone"
  | Skip l -> "()"

let fl = ref 0

let fetch () =
  fl := !fl + 1;
  !fl

module Syntax = struct
  let ( + ) a_1 a_2 = Plus (a_1, a_2)
  let ( - ) a_1 a_2 = Minus (a_1, a_2)
  let ( * ) a_1 a_2 = Times (a_1, a_2)
  let ( < ) a_1 a_2 = Lt (a_1, a_2)
  let ( = ) a_1 a_2 = Eq (a_1, a_2)
  let ( && ) b_1 b_2 = And (b_1, b_2)
  let ( || ) b_1 b_2 = Or (b_1, b_2)
  let not b = Not b
  let ( := ) ?(l = fetch ()) x a = Assign { cnt = (x, a); label = l }
  let ( ^ ) s_1 s_2 = Seq (s_1, s_2)
  let ifte ?(l = fetch ()) b s_1 s_2 = Ifte ({ cnt = b; label = l }, s_1, s_2)
  let whiledo ?(l = fetch ()) b s = While ({ cnt = b; label = l }, s)
  let skip ?(l = fetch ()) () = Skip { cnt = (); label = l }
end
