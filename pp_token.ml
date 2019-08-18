
type token = 
  | WSP of (string)
  | STR of (string)
  | SHARP
  | LPAR
  | RPAR
  | COMMA
  | PUNCT of (string)
  | NUM of (string)
  | LINE
  | NL
  | ID of (string)
  | EOF
  | DEFINE
  | INCLUDE
  | IF
  | IFDEF
  | IFNDEF
  | ELIF
  | ELSE
  | ENDIF
  | SHARP_DEFINE
  | SHARP_INCLUDE
  | SHARP_IF
  | SHARP_IFDEF
  | SHARP_IFNDEF
  | SHARP_ELIF
  | SHARP_ELSE
  | SHARP_ENDIF
  | SHARP_NON_DIRECTIVE of token
  [@@deriving show {with_path=false}]
