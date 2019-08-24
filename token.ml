
type token = 
  | AMP
  | ARROW
  | ASSIGN
  | AST
  | AST_ASSIGN
  | CHAR
  | COMMA
  | DOT
  | DUMMY
  | ELSE
  | ENUM
  | EOF
  | EQ
  | FOR
  | GE
  | GT
  | IDENT of (string)
  | IF
  | INT
  | LBRACE
  | LBRACKET
  | LE
  | LONG
  | LPAR
  | LT
  | MINUS
  | MINUS_ASSIGN
  | MINUSMINUS
  | MOD
  | MOD_ASSIGN
  | NE
  | NUM of (string)
  | PLUS
  | PLUS_ASSIGN
  | PLUSPLUS
  | RBRACE
  | RBRACKET
  | RETURN
  | RPAR
  | SEMI
  | SHORT
  | SIZEOF
  | SLASH
  | SLASH_ASSIGN
  | STR of (string)
  | STRUCT
  | TYPEDEF
  | EXTERN
  | TYPEDEF_ID of (string)
  | UNION
  | WHILE
  [@@deriving show {with_path = false}]

