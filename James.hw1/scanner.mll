{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| '=' { EQUALS }
| ';' { SEQ }
| ['a'-'z']+ as name { VARR(name) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
