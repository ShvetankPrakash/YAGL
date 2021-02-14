type operator = Add | Sub | Mul | Div

type assigner = Equals

type expr =
    Binop of expr * operator * expr
  | Evaluate of expr * expr
  | Assign of string * assigner * expr
  | Lookup of string
  | Lit of int
