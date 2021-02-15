open Ast

module StringHash = Hashtbl.Make(struct
        type t = string
        let equal x y = x = y
        let hash = Hashtbl.hash
end)

let vals = StringHash.create 10

let makevariable = function
    Var(x) -> x
  | _      -> "not a variable"

let rec eval = function 
    Lit(x)            -> x
  | Var(x)            -> StringHash.find vals x
  | Assignment(e1, op, e2) ->
      let v1 = makevariable e1 in
      let v2 = eval e2 in
      let n = (StringHash.add vals v1 v2) in
      (match op with
      Assign -> ignore n; v2) 
  | Sequencing(e1, op, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match op with
      Sequence -> ignore v1; v2)
  | Binop(e1, op, e2) ->
      let v1 = eval e1 in
      let v2 = eval e2 in
      (match op with
	Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  let result = eval expr in
  print_endline (string_of_int result)
