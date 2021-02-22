/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRAC RBRAC COMMA PLUS MINUS TIMES DIVIDE ASSIGN ARROW COLON DOT QMARK
%token NOT EQ NEQ LT LEQ GT GEQ AND OR
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
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
   typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { typ = $1;
	 fname = $2;
	 formals = List.rev $4;
	 locals = List.rev $7;
	 body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    typ ID                   { [($1,$2)]     }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT    { Int    }
  | BOOL   { Bool   }
  | FLOAT  { Float  }
  | VOID   { Void   }
  | CHAR   { Void   }
  | STRING { Void   }
  | NODE   { Void   }
  | GRAPH  { Void   }
  | EDGE   { Void   }
  | typ LBRAC expr RBRAC { Void }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
     typ ID SEMI { ($1, $2) }  /* What does this do? */
   | typ ID ASSIGN expr SEMI { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI                               { Expr $1               }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | BFS LPAREN expr SEMI expr SEMI expr RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

edge:
    ID ARROW LITERAL ID    { Noexpr }
  | ID ARROW LITERAL edge  { Noexpr }

expr:
    LITERAL          { Literal($1)            }
  | FLIT	     { Fliteral($1)           }
  | BLIT             { BoolLit($1)            }
  | CHRLIT           { Noexpr                 }
  | STRLIT           { Noexpr                 }
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
  | ID COLON edge    { Noexpr                 } 
  | ID COLON expr QMARK expr { Noexpr         }
  | ID DOT ID        { Noexpr                 }
  | ID LBRAC expr RBRAC { Noexpr              }
  | ID LPAREN args_opt RPAREN { Call($1, $3)  }
  | LPAREN expr RPAREN { $2                   }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
