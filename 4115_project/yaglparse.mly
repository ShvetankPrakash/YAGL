/* Ocamlyacc parser for YAGL - Yet Another Graph Language 
   Forked from MicroC
*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAC RBRAC COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW COLON DOT QMARK
%token NOT EQ LT GT AND OR
%token RETURN IF ELSE FOR WHILE BFS INT BOOL FLOAT VOID CHAR STRING NODE GRAPH EDGE
%token <int> LITERAL
%token <bool> BLIT
%token <char> CHRLIT
%token <string> STRLIT
%token <string> ID FLIT
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ COLON
%left LT GT
%left ARROW
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT
%left QMARK
%left LBRAC 
%left DOT

%%

program:
  src_file EOF { (List.rev (fst $1), snd $1) }

src_file:
   /* nothing */ { ([], [])               }
 | src_file stmt { (($2 :: fst $1), snd $1) }
 | src_file fdecl { (fst $1, ($2 :: snd $1)) }

typ:
    INT    { Int    }
  | FLOAT  { Float  }
  | STRING { String }
  | VOID   { Void   }
  | BOOL   { Bool   }
  | typ LBRAC expr RBRAC { Array($1, $3) }
  | EDGE   { Edge   }
  | CHAR   { Char   }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 body = List.rev $7 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }
  
stmt_list:
  /* nothing */    { [] }
  | stmt_list stmt { $2::$1 }

graph_stmts:
    NODE ID LPAREN expr RPAREN SEMI         { Binding_Assign((Node, $2), 
                                              Assign($2, NodeLit($2, $4), Noexpr)) }
  | GRAPH ID SEMI                           { Binding_Assign((Graph, $2), 
                                              Assign($2, GraphLit($2), Noexpr)) }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | BFS LPAREN expr SEMI expr SEMI expr RPAREN stmt
                                            { Bfs($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | graph_stmts                             { $1                    }
  | typ ID SEMI                             { Binding($1, $2)       }
  | typ ID ASSIGN expr SEMI                 { Binding_Assign(($1, $2), Assign($2,$4,Noexpr)) }

expr_opt: /* can be expr or nothing */
        /* epsilon/nothing */   { Noexpr }
      | expr                    { $1     }

expr:
    LITERAL          { Literal($1)                       }
  | FLIT             { FLit($1)                          }
  | BLIT             { BoolLit($1)                       }
  | CHRLIT           { ChrLit($1)                        }
  | STRLIT           { StrLit($1)                        }
  | ID               { Id($1)                            }
  | expr PLUS   expr { Binop($1, Add,   $3)              }
  | expr MINUS  expr { Binop($1, Sub,   $3)              }
  | expr TIMES  expr { Binop($1, Mult,  $3)              }
  | expr DIVIDE expr { Binop($1, Div,   $3)              }
  | expr EQ     expr { Binop($1, Equal, $3)              }
  | expr LT     expr { Binop($1, Less,  $3)              }
  | expr GT     expr { Binop($1, Greater, $3)            }
  | expr AND    expr { Binop($1, And,   $3)              }
  | expr OR     expr { Binop($1, Or,    $3)              }
  | MINUS expr %prec NOT { Unop(Neg, $2)                 }
  | NOT expr         { Unop(Not, $2)                     }
  | ID ASSIGN expr   { Assign($1, $3, Noexpr)            }
  | ID LBRAC expr RBRAC ASSIGN expr { Assign($1, $3, $6) }
  /*| ID COLON expr QMARK expr { Noexpr                    }*/
  | ID DOT ID        { Attr($1, $3)                      } 
  | ID LBRAC expr RBRAC { Access($1, $3)                 }
  | LPAREN expr RPAREN { $2                              }
  | ID LPAREN args_opt RPAREN { Call($1, $3)             }
  | expr COLON expr ARROW expr expr 
                          { EdgeOp($1, $3, Link, $5, $6) }
  | expr COLON expr ARROW expr 
                          { EdgeOp($1, $3, Link, Literal(1), $5) }
  /*| expr COLON expr ARROW expr
                          { EdgeOp($1, $3, Link, 1, $5) }*/

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
