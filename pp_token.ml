
type token = 
  | WSP of (string)
  | STR of (string)
  | SHARP
  | LPAR
  | RPAR
  | COMMA
  | PUNCT of (string)
  | NUM of (string)
  | NL
  | ID of (string)
  | EOF
  | DEFINE
  [@@deriving show {with_path=false}]
