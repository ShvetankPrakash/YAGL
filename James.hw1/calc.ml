
module StringHash = Hashtbl.Make(struct type t = string
        let equal x y = x = y
        let hash = Hashtbl.hash
        end)


open Ast


let rec eval = function 
    (map, Lit(x))               -> x
  | (map, String(x))            -> StringHash.find map x
  | (map, Binop(e1, op, e2))    ->
      let v1  = eval(map, e1) in
      let v2 = eval(map, e2) in
      (match op with
	Add -> v1 + v2
      | Sub -> v1 - v2
      | Mul -> v1 * v2
      | Div -> v1 / v2)

   | (map, Assign(n, eqn))      -> 
       (let res = eval (map, eqn)
        in
          (StringHash.add map n res; 
             res))
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let map = StringHash.create 10 in
  while lexbuf.lex_start_pos == 0 
        || String.length (Lexing.lexeme lexbuf) > 0 do
        let expr = Parser.seq Scanner.tokenize lexbuf in
          let result = eval (map, expr) in
            match String.contains (Lexing.lexeme lexbuf) ';' with
                  (true) -> ()
                | (false) -> print_endline (string_of_int result)
  done
