{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| ';' { END_OF_EXPR }
| '=' { EQUALS }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ['a'-'z']+ as lit { VARIABLE(lit) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
