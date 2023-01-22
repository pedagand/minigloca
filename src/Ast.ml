open Printf

(* Arithmetic expressions *)

type a = Int of int 
    | Id of string
    | Plus of a * a
    | Minus of a * a
    | Times of a * a 
    [@@deriving show, eq]

let rec aexpToString a =
    match a with
    | Int(i) -> sprintf "%d" i
    | Id(s) -> sprintf "%s" s
    | Plus(a_1, a_2) -> sprintf "%s + %s " (aexpToString a_1) (aexpToString a_2)
    | Minus(a_1, a_2) -> sprintf "%s - %s " (aexpToString a_1) (aexpToString a_2)
    | Times(a_1, a_2) -> sprintf "%s * %s " (aexpToString a_1) (aexpToString a_2)

(* Boolean expressions *)

type b = True | False
    | Lt of a * a
    | Eq of a * a
    | And of b * b
    | Or of b * b
    | Not of b
    [@@deriving show, eq]

let (<) a_1 a_2 = Lt(a_1, a_2)
let (=) a_1 a_2 = Eq(a_1, a_2)

let (&&) b_1 b_2 = And(b_1, b_2)
let (||) b_1 b_2 = Or(b_1, b_2)
let neg b = Not b

(* Statements *)

type label = int [@@deriving show, eq]

type s = Assign of string * a * label
    | Seq of s * s
    | Ifte of b * s * s * label
    | While of b * s * label
    | Skip 
    [@@deriving show, eq]

type labelled = s * label
