{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '=' { EQUALS }
| ';' { SEQUENCES }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| ['a'-'z']+ as var { VARIABLE(var) }
| eof { EOF }
