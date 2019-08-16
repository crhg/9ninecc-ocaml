
type token = 
  | WSP of (string)
  | STR of (string)
  | SHARP
  | PUNCT of (string)
  | NUM of (string)
  | NL
  | ID of (string)
  | EOF
  | DEFINE
  [@@deriving show {with_path=false}]
