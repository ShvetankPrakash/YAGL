type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRAC
  | RBRAC
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | ARROW
  | COLON
  | DOT
  | QMARK
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | BFS
  | INT
  | BOOL
  | FLOAT
  | VOID
  | CHAR
  | STRING
  | NODE
  | GRAPH
  | EDGE
  | LITERAL of (int)
  | BLIT of (bool)
  | CHRLIT of (char)
  | STRLIT of (string)
  | ID of (string)
  | FLIT of (string)
  | EOF

open Parsing;;
let _ = parse_error;;
# 6 "yaglparse.mly"
open Ast
# 56 "yaglparse.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* LBRAC *);
  263 (* RBRAC *);
  264 (* COMMA *);
  265 (* PLUS *);
  266 (* MINUS *);
  267 (* TIMES *);
  268 (* DIVIDE *);
  269 (* ASSIGN *);
  270 (* ARROW *);
  271 (* COLON *);
  272 (* DOT *);
  273 (* QMARK *);
  274 (* NOT *);
  275 (* EQ *);
  276 (* NEQ *);
  277 (* LT *);
  278 (* LEQ *);
  279 (* GT *);
  280 (* GEQ *);
  281 (* AND *);
  282 (* OR *);
  283 (* RETURN *);
  284 (* IF *);
  285 (* ELSE *);
  286 (* FOR *);
  287 (* WHILE *);
  288 (* BFS *);
  289 (* INT *);
  290 (* BOOL *);
  291 (* FLOAT *);
  292 (* VOID *);
  293 (* CHAR *);
  294 (* STRING *);
  295 (* NODE *);
  296 (* GRAPH *);
  297 (* EDGE *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  298 (* LITERAL *);
  299 (* BLIT *);
  300 (* CHRLIT *);
  301 (* STRLIT *);
  302 (* ID *);
  303 (* FLIT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\004\000\006\000\006\000\009\000\
\009\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
\005\000\005\000\005\000\007\000\007\000\003\000\003\000\008\000\
\008\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\012\000\012\000\013\000\013\000\013\000\013\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\010\000\014\000\014\000\
\015\000\015\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\009\000\000\000\001\000\002\000\
\004\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\001\000\004\000\000\000\002\000\003\000\005\000\000\000\
\002\000\002\000\003\000\003\000\005\000\007\000\009\000\005\000\
\000\000\001\000\006\000\005\000\004\000\003\000\001\000\001\000\
\001\000\001\000\001\000\001\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\002\000\002\000\003\000\
\001\000\005\000\003\000\004\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\002\000\000\000\067\000\000\000\010\000\011\000\012\000\013\000\
\014\000\015\000\016\000\017\000\018\000\001\000\003\000\004\000\
\000\000\000\000\000\000\000\000\000\000\000\000\039\000\041\000\
\042\000\043\000\000\000\040\000\000\000\000\000\022\000\000\000\
\000\000\000\000\054\000\055\000\000\000\000\000\000\000\000\000\
\000\000\019\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\062\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\059\000\000\000\000\000\047\000\048\000\000\000\000\000\000\000\
\000\000\000\000\000\000\038\000\008\000\000\000\000\000\023\000\
\061\000\000\000\060\000\000\000\000\000\037\000\020\000\000\000\
\000\000\000\000\036\000\058\000\000\000\009\000\035\000\021\000\
\000\000\000\000\000\000\024\000\005\000\000\000\000\000\000\000\
\000\000\000\000\025\000\000\000\000\000\000\000\000\000\000\000\
\000\000\026\000\028\000\027\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\032\000\000\000\000\000\000\000\030\000\
\000\000\000\000\031\000"

let yydgoto = "\002\000\
\003\000\004\000\015\000\016\000\017\000\054\000\093\000\098\000\
\055\000\106\000\107\000\110\000\030\000\059\000\060\000"

let yysindex = "\016\000\
\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\003\255\036\255\011\255\036\255\036\255\036\255\000\000\000\000\
\000\000\000\000\212\255\000\000\034\001\250\254\000\000\122\001\
\036\255\213\000\000\000\000\000\036\255\036\255\036\255\195\255\
\247\254\000\000\036\255\036\255\036\255\036\255\036\255\036\255\
\036\255\036\255\036\255\228\254\004\255\039\255\035\255\082\000\
\000\000\108\001\052\255\057\255\054\001\108\001\105\255\072\001\
\000\000\251\254\251\254\000\000\000\000\200\255\048\255\048\255\
\131\001\126\001\023\255\000\000\000\000\069\255\122\001\000\000\
\000\000\036\255\000\000\254\254\036\255\000\000\000\000\005\255\
\108\001\030\255\000\000\000\000\122\001\000\000\000\000\000\000\
\010\255\043\255\022\255\000\000\000\000\036\255\075\255\089\255\
\090\255\101\000\000\000\095\255\108\001\097\255\036\255\036\255\
\036\255\000\000\000\000\000\000\232\000\251\000\120\000\188\255\
\188\255\036\255\067\255\000\000\139\000\188\255\036\255\000\000\
\014\001\188\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\242\255\000\000\000\000\007\000\000\000\098\255\
\000\000\000\000\000\000\000\000\099\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\100\255\000\000\
\000\000\031\255\000\000\103\255\000\000\179\255\090\001\000\000\
\000\000\042\000\063\000\000\000\000\000\202\000\160\000\181\000\
\107\255\148\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\033\255\000\000\000\000\000\000\126\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\111\255\000\000\000\000\
\000\000\000\000\000\000\000\000\115\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\157\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\024\000\000\000\229\255\000\000\000\000\025\000\
\000\000\238\255\198\255\000\000\000\000\000\000\000\000"

let yytablesize = 675
let yytable = "\029\000\
\014\000\034\000\035\000\036\000\053\000\045\000\046\000\052\000\
\018\000\018\000\018\000\031\000\032\000\075\000\056\000\018\000\
\001\000\076\000\058\000\061\000\062\000\064\000\031\000\033\000\
\066\000\067\000\068\000\069\000\070\000\071\000\072\000\073\000\
\074\000\065\000\033\000\066\000\065\000\020\000\065\000\090\000\
\066\000\078\000\079\000\091\000\020\000\021\000\100\000\101\000\
\019\000\077\000\094\000\088\000\021\000\022\000\081\000\099\000\
\043\000\044\000\045\000\046\000\022\000\123\000\124\000\089\000\
\082\000\097\000\092\000\128\000\086\000\102\000\103\000\131\000\
\087\000\104\000\105\000\095\000\111\000\023\000\024\000\025\000\
\026\000\027\000\028\000\109\000\023\000\024\000\025\000\026\000\
\027\000\028\000\112\000\113\000\117\000\118\000\119\000\126\000\
\020\000\116\000\100\000\115\000\006\000\063\000\007\000\125\000\
\021\000\064\000\037\000\052\000\129\000\052\000\038\000\033\000\
\022\000\052\000\052\000\034\000\096\000\039\000\084\000\040\000\
\041\000\102\000\103\000\052\000\108\000\104\000\105\000\024\000\
\000\000\024\000\024\000\052\000\052\000\000\000\000\000\024\000\
\023\000\024\000\025\000\026\000\027\000\028\000\000\000\024\000\
\000\000\000\000\000\000\000\000\053\000\000\000\053\000\000\000\
\024\000\024\000\053\000\053\000\024\000\024\000\029\000\000\000\
\029\000\029\000\000\000\000\000\053\000\000\000\029\000\024\000\
\024\000\024\000\024\000\024\000\024\000\053\000\029\000\000\000\
\000\000\000\000\000\000\056\000\000\000\056\000\000\000\029\000\
\029\000\056\000\056\000\029\000\029\000\020\000\000\000\100\000\
\000\000\000\000\000\000\056\000\020\000\021\000\029\000\029\000\
\029\000\029\000\029\000\029\000\021\000\022\000\000\000\000\000\
\043\000\044\000\045\000\046\000\022\000\037\000\102\000\103\000\
\000\000\038\000\104\000\105\000\048\000\000\000\049\000\000\000\
\039\000\000\000\040\000\041\000\000\000\023\000\024\000\025\000\
\026\000\027\000\028\000\000\000\023\000\024\000\025\000\026\000\
\063\000\028\000\044\000\000\000\044\000\000\000\000\000\000\000\
\044\000\044\000\044\000\044\000\044\000\044\000\000\000\000\000\
\000\000\000\000\044\000\000\000\044\000\000\000\044\000\057\000\
\044\000\057\000\044\000\044\000\000\000\057\000\057\000\057\000\
\057\000\057\000\057\000\000\000\000\000\000\000\000\000\057\000\
\000\000\057\000\000\000\057\000\000\000\057\000\000\000\057\000\
\057\000\005\000\006\000\007\000\008\000\009\000\010\000\011\000\
\012\000\013\000\045\000\000\000\045\000\000\000\000\000\000\000\
\045\000\045\000\045\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\045\000\000\000\045\000\000\000\045\000\046\000\
\045\000\046\000\045\000\045\000\000\000\046\000\046\000\046\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\046\000\
\000\000\046\000\080\000\046\000\000\000\046\000\000\000\046\000\
\046\000\000\000\043\000\044\000\045\000\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\114\000\048\000\000\000\
\049\000\000\000\050\000\051\000\000\000\043\000\044\000\045\000\
\046\000\000\000\000\000\000\000\000\000\000\000\000\000\047\000\
\122\000\048\000\000\000\049\000\000\000\050\000\051\000\000\000\
\043\000\044\000\045\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\127\000\048\000\000\000\049\000\000\000\
\050\000\051\000\000\000\043\000\044\000\045\000\046\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\000\000\048\000\
\050\000\049\000\050\000\050\000\051\000\000\000\050\000\050\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\050\000\000\000\050\000\000\000\050\000\051\000\050\000\051\000\
\050\000\050\000\000\000\051\000\051\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\051\000\000\000\051\000\
\000\000\051\000\049\000\051\000\049\000\051\000\051\000\000\000\
\049\000\049\000\000\000\000\000\000\000\000\000\000\000\057\000\
\000\000\000\000\049\000\000\000\049\000\043\000\044\000\045\000\
\046\000\000\000\049\000\049\000\000\000\000\000\000\000\047\000\
\000\000\048\000\120\000\049\000\000\000\050\000\051\000\000\000\
\043\000\044\000\045\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\047\000\000\000\048\000\121\000\049\000\000\000\
\050\000\051\000\000\000\043\000\044\000\045\000\046\000\000\000\
\000\000\000\000\000\000\000\000\000\000\047\000\000\000\048\000\
\130\000\049\000\000\000\050\000\051\000\000\000\043\000\044\000\
\045\000\046\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\000\000\048\000\000\000\049\000\000\000\050\000\051\000\
\042\000\000\000\043\000\044\000\045\000\046\000\000\000\000\000\
\000\000\000\000\000\000\000\000\047\000\000\000\048\000\000\000\
\049\000\000\000\050\000\051\000\083\000\000\000\043\000\044\000\
\045\000\046\000\000\000\000\000\000\000\000\000\000\000\000\000\
\047\000\000\000\048\000\000\000\049\000\000\000\050\000\051\000\
\043\000\044\000\045\000\046\000\000\000\000\000\000\000\000\000\
\085\000\000\000\047\000\000\000\048\000\000\000\049\000\000\000\
\050\000\051\000\044\000\044\000\044\000\044\000\000\000\000\000\
\000\000\000\000\044\000\000\000\044\000\000\000\044\000\000\000\
\044\000\000\000\044\000\044\000\043\000\044\000\045\000\046\000\
\000\000\000\000\000\000\000\000\000\000\000\000\047\000\000\000\
\048\000\000\000\049\000\000\000\050\000\051\000\043\000\044\000\
\045\000\046\000\000\000\043\000\044\000\045\000\046\000\000\000\
\047\000\000\000\048\000\000\000\049\000\047\000\050\000\048\000\
\000\000\049\000\005\000\006\000\007\000\008\000\009\000\010\000\
\011\000\012\000\013\000"

let yycheck = "\018\000\
\000\000\020\000\021\000\022\000\032\000\011\001\012\001\014\001\
\006\001\006\001\006\001\001\001\002\001\042\001\033\000\006\001\
\001\000\046\001\037\000\038\000\039\000\040\000\001\001\013\001\
\043\000\044\000\045\000\046\000\047\000\048\000\049\000\050\000\
\051\000\003\001\013\001\003\001\046\001\002\001\008\001\042\001\
\008\001\003\001\008\001\046\001\002\001\010\001\004\001\005\001\
\046\001\046\001\046\001\079\000\010\001\018\001\003\001\046\001\
\009\001\010\001\011\001\012\001\018\001\120\000\121\000\082\000\
\008\001\093\000\085\000\126\000\046\001\027\001\028\001\130\000\
\004\001\031\001\032\001\046\001\002\001\042\001\043\001\044\001\
\045\001\046\001\047\001\102\000\042\001\043\001\044\001\045\001\
\046\001\047\001\002\001\002\001\111\000\112\000\113\000\029\001\
\002\001\001\001\004\001\005\001\003\001\003\001\003\001\122\000\
\010\001\003\001\002\001\001\001\127\000\003\001\006\001\001\001\
\018\001\007\001\008\001\001\001\093\000\013\001\014\001\015\001\
\016\001\027\001\028\001\017\001\100\000\031\001\032\001\002\001\
\255\255\004\001\005\001\025\001\026\001\255\255\255\255\010\001\
\042\001\043\001\044\001\045\001\046\001\047\001\255\255\018\001\
\255\255\255\255\255\255\255\255\001\001\255\255\003\001\255\255\
\027\001\028\001\007\001\008\001\031\001\032\001\002\001\255\255\
\004\001\005\001\255\255\255\255\017\001\255\255\010\001\042\001\
\043\001\044\001\045\001\046\001\047\001\026\001\018\001\255\255\
\255\255\255\255\255\255\001\001\255\255\003\001\255\255\027\001\
\028\001\007\001\008\001\031\001\032\001\002\001\255\255\004\001\
\255\255\255\255\255\255\017\001\002\001\010\001\042\001\043\001\
\044\001\045\001\046\001\047\001\010\001\018\001\255\255\255\255\
\009\001\010\001\011\001\012\001\018\001\002\001\027\001\028\001\
\255\255\006\001\031\001\032\001\021\001\255\255\023\001\255\255\
\013\001\255\255\015\001\016\001\255\255\042\001\043\001\044\001\
\045\001\046\001\047\001\255\255\042\001\043\001\044\001\045\001\
\046\001\047\001\001\001\255\255\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\255\255\021\001\001\001\
\023\001\003\001\025\001\026\001\255\255\007\001\008\001\009\001\
\010\001\011\001\012\001\255\255\255\255\255\255\255\255\017\001\
\255\255\019\001\255\255\021\001\255\255\023\001\255\255\025\001\
\026\001\033\001\034\001\035\001\036\001\037\001\038\001\039\001\
\040\001\041\001\001\001\255\255\003\001\255\255\255\255\255\255\
\007\001\008\001\009\001\010\001\255\255\255\255\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\255\255\021\001\001\001\
\023\001\003\001\025\001\026\001\255\255\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\255\255\019\001\001\001\021\001\255\255\023\001\255\255\025\001\
\026\001\255\255\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\255\255\255\255\019\001\001\001\021\001\255\255\
\023\001\255\255\025\001\026\001\255\255\009\001\010\001\011\001\
\012\001\255\255\255\255\255\255\255\255\255\255\255\255\019\001\
\001\001\021\001\255\255\023\001\255\255\025\001\026\001\255\255\
\009\001\010\001\011\001\012\001\255\255\255\255\255\255\255\255\
\255\255\255\255\019\001\001\001\021\001\255\255\023\001\255\255\
\025\001\026\001\255\255\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\255\255\255\255\255\255\019\001\255\255\021\001\
\001\001\023\001\003\001\025\001\026\001\255\255\007\001\008\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\017\001\255\255\019\001\255\255\021\001\001\001\023\001\003\001\
\025\001\026\001\255\255\007\001\008\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\017\001\255\255\019\001\
\255\255\021\001\001\001\023\001\003\001\025\001\026\001\255\255\
\007\001\008\001\255\255\255\255\255\255\255\255\255\255\003\001\
\255\255\255\255\017\001\255\255\019\001\009\001\010\001\011\001\
\012\001\255\255\025\001\026\001\255\255\255\255\255\255\019\001\
\255\255\021\001\003\001\023\001\255\255\025\001\026\001\255\255\
\009\001\010\001\011\001\012\001\255\255\255\255\255\255\255\255\
\255\255\255\255\019\001\255\255\021\001\003\001\023\001\255\255\
\025\001\026\001\255\255\009\001\010\001\011\001\012\001\255\255\
\255\255\255\255\255\255\255\255\255\255\019\001\255\255\021\001\
\003\001\023\001\255\255\025\001\026\001\255\255\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\255\255\021\001\255\255\023\001\255\255\025\001\026\001\
\007\001\255\255\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\255\255\255\255\019\001\255\255\021\001\255\255\
\023\001\255\255\025\001\026\001\007\001\255\255\009\001\010\001\
\011\001\012\001\255\255\255\255\255\255\255\255\255\255\255\255\
\019\001\255\255\021\001\255\255\023\001\255\255\025\001\026\001\
\009\001\010\001\011\001\012\001\255\255\255\255\255\255\255\255\
\017\001\255\255\019\001\255\255\021\001\255\255\023\001\255\255\
\025\001\026\001\009\001\010\001\011\001\012\001\255\255\255\255\
\255\255\255\255\017\001\255\255\019\001\255\255\021\001\255\255\
\023\001\255\255\025\001\026\001\009\001\010\001\011\001\012\001\
\255\255\255\255\255\255\255\255\255\255\255\255\019\001\255\255\
\021\001\255\255\023\001\255\255\025\001\026\001\009\001\010\001\
\011\001\012\001\255\255\009\001\010\001\011\001\012\001\255\255\
\019\001\255\255\021\001\255\255\023\001\019\001\025\001\021\001\
\255\255\023\001\033\001\034\001\035\001\036\001\037\001\038\001\
\039\001\040\001\041\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  LBRAC\000\
  RBRAC\000\
  COMMA\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  ARROW\000\
  COLON\000\
  DOT\000\
  QMARK\000\
  NOT\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  AND\000\
  OR\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  BFS\000\
  INT\000\
  BOOL\000\
  FLOAT\000\
  VOID\000\
  CHAR\000\
  STRING\000\
  NODE\000\
  GRAPH\000\
  EDGE\000\
  EOF\000\
  "

let yynames_block = "\
  LITERAL\000\
  BLIT\000\
  CHRLIT\000\
  STRLIT\000\
  ID\000\
  FLIT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    Obj.repr(
# 40 "yaglparse.mly"
            ( _1 )
# 434 "yaglparse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    Obj.repr(
# 43 "yaglparse.mly"
                 ( ([], [])               )
# 440 "yaglparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 44 "yaglparse.mly"
               ( ((_2 :: fst _1), snd _1) )
# 448 "yaglparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decls) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 45 "yaglparse.mly"
               ( (fst _1, (_2 :: snd _1)) )
# 456 "yaglparse.ml"
               : 'decls))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 8 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 49 "yaglparse.mly"
     ( { typ = _1;
	 fname = _2;
	 formals = List.rev _4;
	 locals = List.rev _7;
	 body = List.rev _8 } )
# 471 "yaglparse.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 56 "yaglparse.mly"
                  ( [] )
# 477 "yaglparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 57 "yaglparse.mly"
                  ( _1 )
# 484 "yaglparse.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 60 "yaglparse.mly"
                             ( [(_1,_2)]     )
# 492 "yaglparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 61 "yaglparse.mly"
                             ( (_3,_4) :: _1 )
# 501 "yaglparse.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 64 "yaglparse.mly"
           ( Int    )
# 507 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 65 "yaglparse.mly"
           ( Bool   )
# 513 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 66 "yaglparse.mly"
           ( Float  )
# 519 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "yaglparse.mly"
           ( Void   )
# 525 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "yaglparse.mly"
           ( Void   )
# 531 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 69 "yaglparse.mly"
           ( Void   )
# 537 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "yaglparse.mly"
           ( Void   )
# 543 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "yaglparse.mly"
           ( Void   )
# 549 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "yaglparse.mly"
           ( Void   )
# 555 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 73 "yaglparse.mly"
                         ( Void )
# 563 "yaglparse.ml"
               : 'typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "yaglparse.mly"
                     ( [] )
# 569 "yaglparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 77 "yaglparse.mly"
                     ( _2 :: _1 )
# 577 "yaglparse.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 80 "yaglparse.mly"
                 ( (_1, _2) )
# 585 "yaglparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'typ) in
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 81 "yaglparse.mly"
                             ( (_1, _2) )
# 594 "yaglparse.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 84 "yaglparse.mly"
                   ( [] )
# 600 "yaglparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 85 "yaglparse.mly"
                   ( _2 :: _1 )
# 608 "yaglparse.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 88 "yaglparse.mly"
                                            ( Expr _1               )
# 615 "yaglparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr_opt) in
    Obj.repr(
# 89 "yaglparse.mly"
                                            ( Return _2             )
# 622 "yaglparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 90 "yaglparse.mly"
                                            ( Block(List.rev _2)    )
# 629 "yaglparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 91 "yaglparse.mly"
                                            ( If(_3, _5, Block([])) )
# 637 "yaglparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 92 "yaglparse.mly"
                                            ( If(_3, _5, _7)        )
# 646 "yaglparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 94 "yaglparse.mly"
                                            ( For(_3, _5, _7, _9)   )
# 656 "yaglparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 95 "yaglparse.mly"
                                            ( While(_3, _5)         )
# 664 "yaglparse.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 98 "yaglparse.mly"
                  ( Noexpr )
# 670 "yaglparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 99 "yaglparse.mly"
                  ( _1 )
# 677 "yaglparse.ml"
               : 'expr_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 102 "yaglparse.mly"
                                    ( EdgeOp(_1, _3, Plus, _5, _6) )
# 687 "yaglparse.ml"
               : 'edge))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 103 "yaglparse.mly"
                                    ( EdgeOp(_1, _3, Plus, 1, _5) )
# 696 "yaglparse.ml"
               : 'edge))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'edge) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 104 "yaglparse.mly"
                                    ( ChainedEdgeOp(_1, Plus, _3, _4) )
# 705 "yaglparse.ml"
               : 'edge))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'edge) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 105 "yaglparse.mly"
                                    ( ChainedEdgeOp(_1, Plus, 1, _3) )
# 713 "yaglparse.ml"
               : 'edge))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 108 "yaglparse.mly"
                     ( Literal(_1)            )
# 720 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 109 "yaglparse.mly"
                    ( Fliteral(_1)           )
# 727 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 110 "yaglparse.mly"
                     ( BoolLit(_1)            )
# 734 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 111 "yaglparse.mly"
                     ( Noexpr                 )
# 741 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "yaglparse.mly"
                     ( Noexpr                 )
# 748 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 113 "yaglparse.mly"
                     ( Id(_1)                 )
# 755 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 114 "yaglparse.mly"
                     ( Binop(_1, Add,   _3)   )
# 763 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 115 "yaglparse.mly"
                     ( Binop(_1, Sub,   _3)   )
# 771 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 116 "yaglparse.mly"
                     ( Binop(_1, Mult,  _3)   )
# 779 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 117 "yaglparse.mly"
                     ( Binop(_1, Div,   _3)   )
# 787 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 118 "yaglparse.mly"
                     ( Binop(_1, Equal, _3)   )
# 795 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 119 "yaglparse.mly"
                     ( Binop(_1, Less,  _3)   )
# 803 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 120 "yaglparse.mly"
                     ( Binop(_1, Greater, _3) )
# 811 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 121 "yaglparse.mly"
                     ( Binop(_1, And,   _3)   )
# 819 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 122 "yaglparse.mly"
                     ( Binop(_1, Or,    _3)   )
# 827 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 123 "yaglparse.mly"
                         ( Unop(Neg, _2)      )
# 834 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 124 "yaglparse.mly"
                     ( Unop(Not, _2)          )
# 841 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 125 "yaglparse.mly"
                     ( Assign(_1, _3)         )
# 849 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'edge) in
    Obj.repr(
# 126 "yaglparse.mly"
                     ( _1                     )
# 856 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 127 "yaglparse.mly"
                             ( Noexpr         )
# 865 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 128 "yaglparse.mly"
                     ( Noexpr                 )
# 873 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 129 "yaglparse.mly"
                        ( Noexpr              )
# 881 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'args_opt) in
    Obj.repr(
# 130 "yaglparse.mly"
                              ( Call(_1, _3)  )
# 889 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 131 "yaglparse.mly"
                       ( _2                   )
# 896 "yaglparse.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 134 "yaglparse.mly"
                  ( [] )
# 902 "yaglparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'args_list) in
    Obj.repr(
# 135 "yaglparse.mly"
               ( List.rev _1 )
# 909 "yaglparse.ml"
               : 'args_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 138 "yaglparse.mly"
                            ( [_1] )
# 916 "yaglparse.ml"
               : 'args_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'args_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 139 "yaglparse.mly"
                         ( _3 :: _1 )
# 924 "yaglparse.ml"
               : 'args_list))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
