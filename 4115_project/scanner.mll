(* Ocamllex scanner for MicroC *)

{ open Microcparse }

let digit = ['0' - '9']
let digits = digit+
let ascii = [' ' - '~']
let escapeChars = ('\\' ['b' 't' 'r' 'n'])

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| "["      { LBRAC }
| "]"      { RBRAC }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '='      { ASSIGN }
| "=="     { EQ }
| '<'      { LT }
| ">"      { GT }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| ":"      { COLON }
| "->"     { ARROW }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "bfs"    { BFS }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "Graph"  { GRAPH }
| "Array"  { ARRAY }
| "String" { STRING }
| "Node"   { NODE }
| "Edge"   { EDGE }
| "void"   { VOID }
(* TODO: We have 1 shift error -> ambiguous we missed *)
(* TODO: Must add . for accessing data inside a Node, Edge, or Graph *)
(* TODO CON'T: EX: N.info1 E.src E.dest E.attr G.nodes G.edges *)
(* Discuss: How do we do this? I think its best if we just add a . -> DOT *)
(* Token and in the parser we can register NODE DOT ID as valid syntax *)
(* Then we will later add the semantics that verifies ID is correct. *)

(* TODO: Must add declaration of a graph EX: Graph G *)

(* TODO: Must add adding node to graph EX: G = G + A *)

(* OPTIONAL: *)
(* TODO: Adding an array of nodes to graph. EX: G = G + A where A is an array *)

(* TODO: Must add retrieving an edge in a graph EX: G: A ? B where A and B are nodes*)

(* TODO: Adding syntax for old-school fixed-size arrays EX: int[5] x; Node[22] n; ... *)
(* Idea: in parser it should be typ LBRAC num(<- IDK where this is defined) RBRAC ID*)

(* TODO: Need to add String declaration *)

(* TODO: Need to add accessing element in array *)

(* TODO: Handle functions ...? *)
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| "'" escapeChars "'"                                 as lxm { CHRLIT(String.get lxm 0) } (* TODO: Fix taking in of two chars for escapeChars *)
| "'" ascii "'"                                       as lxm { CHRLIT(String.get lxm 0) }
| '"' (ascii* escapeChars*)+ '"'                      as lxm { STRLIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
