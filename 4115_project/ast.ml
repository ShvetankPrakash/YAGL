(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Less | Greater |
          And | Or (* | Arrow | Colon *)

type uop = Neg | Not

type expr =
    Literal of int
  | FLit of string
  | BoolLit of bool
  | StrLit of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr * expr
  | Call of string * expr list
  | Attr of string * string
  | Access of string * expr
  | Noexpr

type typ = Void | Int | String | Float | Bool | Array of typ * expr (* For now only testing ints *)

type bind = typ * string

type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | Bfs of expr * expr * expr * stmt
  | While of expr * stmt
  | Binding of bind      (* Only for vdecls *)
  | Binding_Assign of bind * expr
  | Return of expr

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = stmt list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Less -> "<"
  | Greater -> ">"
  | And -> "&&"
  | Or -> "||"
  (*
  | Arrow -> "->"
  | Colon -> ":"
  *)

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | FLit(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StrLit(str) -> str
  | Id(s) -> s
  | Attr(s, a) -> s ^ "." ^ a
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e1, e2) -> 
      (match e2 with 
         Noexpr -> v ^ " = " ^ string_of_expr e1
       | _ -> v ^ "[" ^ string_of_expr e1 ^ "]" ^ " = " ^ string_of_expr e2 
      )
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Access(id, e) -> id ^ "[" ^ string_of_expr e ^ "]"
  | Noexpr -> ""
  (*
  | EdgeOp(e1, e2, o, e3, e4) -> string_of_expr e1 ^ " "
    ^ string_of_expr e2 ^ " " ^ string_of_op o ^ " " 
    ^ string_of_expr e3 ^ " " ^ string_of_expr e4
  | ChainedEdgeOp(e1, o, e2, e3) -> string_of_expr e1 ^ " "
    ^ string_of_op o ^ " " 
    ^ string_of_expr e2 ^ " " ^ string_of_expr e3
  | NodeOfGraph(e1, e2) -> "(" ^  e1 ^ ", " 
    ^ e2 ^ ")"
  *)

let rec string_of_typ = function
    Void        -> "void"
  | Int         -> "int"
  | Float       -> "float"
  | String      -> "String"
  | Bool        -> "bool"
  | Array(t, e) -> string_of_typ t ^ "[" ^ string_of_expr e ^ "]"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | Bfs(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Binding(t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n" 
  | Binding_Assign((t, id), e) -> 
        match e with
                Assign(_, e',_) -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e' ^ ";\n"
                | _           -> string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr e  ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (stmts, funcs) =
  String.concat "" (List.map string_of_stmt stmts) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

