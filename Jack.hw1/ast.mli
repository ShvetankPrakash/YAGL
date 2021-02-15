type operator = Add | Sub | Mul | Div

type equalsign = Assign
type semicolon = Sequence

type expr =
    Binop of expr * operator * expr
  | Assignment of expr * equalsign * expr
  | Sequencing of expr * semicolon * expr
  | Var of string
  | Lit of int
