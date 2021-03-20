(* Semantic checking for the YAGL compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each statement *)


let check stmts =

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, ty) = StringMap.add name {
      typ = Void;
      fname = name; 
      formals = [(ty, "x")];
      locals = []; body = [] } map
    in List.fold_left add_bind StringMap.empty [ ("print", Int) ]
  in

  (* Collect all function names into one symbol table *)
  let function_decls = built_in_decls 
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  
  let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
  in
    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
         Call(fname, args) as call -> 
          let fd = find_func fname in
          let param_length = List.length fd.formals in
          if List.length args != param_length then
            raise (Failure ("expecting " ^ string_of_int param_length ^ 
                            " arguments in " ^ string_of_expr call))
          else let check_call (ft, _) e = 
            let (et, e') = expr e in 
            let err = "illegal argument found " ^ string_of_typ et ^
              " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in (check_assign ft et err, e')
          in 
          let args' = List.map2 check_call fd.formals args
          in (fd.typ, SCall(fname, args'))
       | Literal  l -> (Int, SLiteral l)
       | _ -> raise (Failure("Error 1: Ints only and calls are supported exressions currently.")) 
    in 

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let check_stmt = function
        Expr e -> SExpr (expr e)
      | _      -> raise (Failure("Error 2: Expressions only supported statements currently.")) 
    
  in 
  
  let correct_stmts = List.map check_stmt stmts 

  in [{ 
       styp = Void;
       sfname = "main"; 
       sformals = [];
       slocals = []; 
       sbody = correct_stmts
     }]
