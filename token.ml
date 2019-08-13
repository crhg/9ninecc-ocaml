
type token = 
  | WHILE
  | UNION
  | TYPEDEF_ID of (string)
  | TYPEDEF
  | STRUCT
  | STR of (string)
  | SLASH
  | SIZEOF
  | SHORT
  | SEMI
  | RPAR
  | RETURN
  | RBRACKET
  | RBRACE
  | PLUS
  | NUM of (string)
  | NE
  | MINUS
  | LT
  | LPAR
  | LONG
  | LE
  | LBRACKET
  | LBRACE
  | INT
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FOR
  | EQ
  | EOF
  | ELSE
  | DUMMY
  | DOT
  | COMMA
  | CHAR
  | AST
  | ASSIGN
  | ARROW
  | AMP
  [@@deriving show {with_path = false}]

