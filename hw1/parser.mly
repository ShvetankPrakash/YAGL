%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF EQUALS SEQUENCES
%token <string> VARIABLE
%token <int> LITERAL

%left SEQUENCES
%right EQUALS
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) } 
| expr EQUALS expr { Assignment($1, Assign, $3) }
| expr SEQUENCES expr { Sequencing($1, Sequence, $3) }
| LITERAL          { Lit($1) }
| VARIABLE         { Var($1) }
