%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF EQUALS SEQ
%token <int> LITERAL
%token <string> VARR

%left SEQ
%right EQUALS
%left PLUS MINUS
%left TIMES DIVIDE

%start seq
%type <Ast.expr> expr
%type <Ast.expr> seq

%%
seq:
  expr SEQ { $1 }
| expr     { $1 }

expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| VARR EQUALS expr { Assign($1, $3)     }
| LITERAL          { Lit($1)            }
| VARR             { String($1)         }
