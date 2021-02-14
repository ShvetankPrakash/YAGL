%{ open Ast %}

%token END_OF_EXPR EQUALS PLUS MINUS TIMES DIVIDE EOF
%token <string> VARIABLE
%token <int> LITERAL

%left END_OF_EXPR
%right EQUALS
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:

| expr END_OF_EXPR expr { Evaluate($1, $3) }
| VARIABLE EQUALS expr { Assign($1, Equals, $3) }
| expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| VARIABLE         { Lookup($1) }
| LITERAL          { Lit($1) }

