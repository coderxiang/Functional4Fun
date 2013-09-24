type token =
  | DEFINE
  | DEFINEREC
  | LAMBDA
  | LET
  | LETREC
  | IF
  | LPAREN
  | RPAREN
  | DELAY
  | FORCE
  | DOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | NOT
  | PLUS
  | MINUS
  | MUL
  | DIV
  | MOD
  | LIST
  | CONS
  | CAR
  | CDR
  | NIL
  | NULL
  | TRUE
  | FALSE
  | LOAD
  | ID of (string)
  | INT of (int)
  | STRING of (string)
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr list
