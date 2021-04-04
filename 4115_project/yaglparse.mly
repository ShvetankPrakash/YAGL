/* Ocamlyacc parser for YAGL - Yet Another Graph Language 
   Forked from MicroC
*/

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAC RBRAC COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW COLON DOT QMARK
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
%token RETURN IF ELSE FOR WHILE BFS INT BOOL FLOAT VOID CHAR STRING NODE GRAPH EDGE
%token <int> LITERAL
%token <float> FLIT
%token <bool> BLIT
%token <char> CHRLIT
%token <string> STRLIT
%token <string> ID /* FLIT Note: not sure what "FLIT" was referring to here */
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ COLON
%left LT GT LEQ GEQ
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
/*
  | CHAR   { Void   }
  | NODE   { Void   }
  | GRAPH  { Void   }
  | EDGE   { Void   }
  | typ LBRAC expr RBRAC { Void }
*/
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

vdecl:
     typ ID SEMI { ($1, $2) } 

/*
Add variable assignment in same stmt
vdecl:
     typ ID SEMI { ($1, $2, Noexpr) } 
   | typ ID ASSIGN expr SEMI { ($1, $2, $3) }
*/

stmt_list:
  /* nothing */    { [] }
  | stmt_list stmt { $2::$1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | BFS LPAREN expr SEMI expr SEMI expr RPAREN stmt
                                            { Bfs($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | vdecl                                   { Binding($1)           }

expr_opt: /* can be expr or nothing */
        /* epsilon/nothing */   { Noexpr }
      | expr                    { $1     }



expr:
    LITERAL          { Literal($1)            }
  | FLIT             { FLit($1)               }
  | BLIT             { BoolLit($1)            }
  | CHRLIT           { Noexpr                 }
  | STRLIT           { StrLit($1)             }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr TIMES  expr { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr { Binop($1, Div,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | MINUS expr %prec NOT { Unop(Neg, $2)      }
  | NOT expr         { Unop(Not, $2)          }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | ID COLON expr QMARK expr { Noexpr         }
  | ID DOT ID        { Noexpr                 }
  | ID LBRAC expr RBRAC { Noexpr              }
  | LPAREN expr RPAREN { $2                   }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }  

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
