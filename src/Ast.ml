(* Arithmetic expressions *)

type a =
  | Int of int
  | Id of string
  | Plus of a * a
  | Minus of a * a
  | Times of a * a
[@@deriving show, eq]

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
