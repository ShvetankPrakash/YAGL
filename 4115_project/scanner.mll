(* Ocamllex scanner for YAGL *)

{ open Yaglparse }

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
| "String" { STRING }
| "Node"   { NODE }
| "Edge"   { EDGE }
| "void"   { VOID }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(float_of_string lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| "'" escapeChars "'"                                 as lxm { CHRLIT(String.get lxm 0) } 
| "'" ascii "'"                                       as lxm { CHRLIT(String.get lxm 0) }    (* handle escape chars *)
| '"' (( ascii # '"' )* escapeChars*)+ '"'                      as lxm { STRLIT(String.sub lxm 1 ((String.length lxm )-2) ) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
