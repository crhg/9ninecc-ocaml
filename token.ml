
type token = 
  | AMP
  | AMP_ASSIGN
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
  | LAND
  | LBRACE
  | LBRACKET
  | LE
  | LONG
  | LOR
  | LPAR
  | LT
  | MINUS
  | MINUS_ASSIGN
  | MINUSMINUS
  | MOD
  | MOD_ASSIGN
  | NE
  | NUM of (string)
  | OR
  | OR_ASSIGN
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
  | XOR
  | XOR_ASSIGN
  [@@deriving show {with_path = false}]

