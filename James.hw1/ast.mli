type operator = Add | Sub | Mul | Div

type expr =
    Binop       of expr * operator * expr
  | Assign      of string * expr
  | Lit         of int
  | String      of string
