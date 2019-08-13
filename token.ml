
type token = 
  | AMP
  | ARROW
  | ASSIGN
  | AST
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
  | NE
  | NUM of (string)
  | PLUS
  | RBRACE
  | RBRACKET
  | RETURN
  | RPAR
  | SEMI
  | SHORT
  | SIZEOF
  | SLASH
  | STR of (string)
  | STRUCT
  | TYPEDEF
  | TYPEDEF_ID of (string)
  | UNION
  | WHILE
  [@@deriving show {with_path = false}]

