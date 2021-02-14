open Ast

exception VariableNotInitialized of string * string;;

module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
end);;

let vals = StringHash.create 10;;

let rec eval = function 
    Lit(x)            -> x
  | Lookup(x)         -> 
      (match StringHash.mem vals x with
      true -> StringHash.find vals x
      | false -> raise (VariableNotInitialized(x, "not initialized")))
  | Evaluate(e1, e2) -> 
      let v1 = eval e1 in
      let v2 = eval e2 in 
      (match v1, v2 with
      _, _ -> v2)
  | Assign(e1, op, e2) ->
      let v2 = eval e2 in
      (match op with
      Equals -> StringHash.add vals e1 v2; v2)
  | Binop(e1, op, e2) ->
      let v1  = eval e1 in
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

  (* In parser.mly: Took out | expr END_OF_EXPR { $1 } for the second line of the expr statement - 
  would have correctly parsed a ; at the end of a line *)
