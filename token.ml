
type token = 
  | AMP
  | AMP_ASSIGN
  | ARROW
  | ASSIGN
  | AST
  | AST_ASSIGN
  | BOOL
  | BREAK
  | CASE
  | CHAR
  | COLON
  | COMMA
  | CONTINUE
  | DEFAULT
  | DO
  | DOT
  | DOTS
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
  | LSHIFT
  | LSHIFT_ASSIGN
  | LT
  | MINUS
  | MINUS_ASSIGN
  | MINUSMINUS
  | MOD
  | MOD_ASSIGN
  | NE
  | NOT
  | NUM of (string)
  | OR
  | OR_ASSIGN
  | PLUS
  | PLUS_ASSIGN
  | PLUSPLUS
  | QUESTION
  | RBRACE
  | RBRACKET
  | RETURN
  | RPAR
  | RSHIFT
  | RSHIFT_ASSIGN
  | SEMI
  | SHORT
  | SIZEOF
  | SLASH
  | SLASH_ASSIGN
  | STATIC
  | STR of (string)
  | STRUCT
  | SWITCH
  | TILDA
  | TYPEDEF
  | EXTERN
  | TYPEDEF_ID of (string)
  | UNION
  | VOID
  | WHILE
  | XOR
  | XOR_ASSIGN
  [@@deriving show {with_path = false}]

