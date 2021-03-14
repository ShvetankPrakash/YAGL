type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRAC
  | RBRAC
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | ARROW
  | COLON
  | DOT
  | QMARK
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | BFS
  | INT
  | BOOL
  | FLOAT
  | VOID
  | CHAR
  | STRING
  | NODE
  | GRAPH
  | EDGE
  | LITERAL of (int)
  | BLIT of (bool)
  | CHRLIT of (char)
  | STRLIT of (string)
  | ID of (string)
  | FLIT of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
