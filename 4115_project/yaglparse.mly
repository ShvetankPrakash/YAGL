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
  stmt_list EOF { List.rev $1 }

/* 
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
*/

stmt_list:
  /* nothing */    { [] }
  | stmt_list stmt { $2::$1 }

stmt:
    expr SEMI                               { Expr $1               }
  /* | RETURN expr_opt SEMI                 { Return $2             } */
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | BFS LPAREN expr SEMI expr SEMI expr RPAREN stmt
                                            { Bfs($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

/* Need to add func_stmt which will be stmt | return expr */

expr:
    LITERAL          { Literal($1)            }
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
