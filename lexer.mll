
{
  open Parser;;
  exception Lexical_error;; 
}

rule token = parse
    [' ' '\t']  { token lexbuf }
  | "lambda"    { LAMBDA }
  | "L"         { LAMBDA }
  | "true"      { TRUE }
  | "false"     { FALSE }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "succ"      { SUCC }
  | "pred"      { PRED }
  | "iszero"    { ISZERO }
  | "let"       { LET }
  | "letrec"    { LETREC }
  | "in"        { IN }
  | "concat"    { CONCAT }
  | "Bool"      { BOOL }
  | "String"    { STRING }
  | "Nat"       { NAT }
  | '('         { LPAREN }
  | ')'         { RPAREN }
  | '{'         { LBRACKET }
  | '}'         { RBRACKET }
  | ','         { COMMA }
  | '.'         { DOT }
  | '='         { EQ }
  | ':'         { COLON }
  | "->"        { ARROW }
  | ['0'-'9']+  { INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | ['a'-'z']['a'-'z' '_' '0'-'9']*
                { STRINGV (Lexing.lexeme lexbuf) }
  | '"' [^ '"' ';' '\n']* '"' { STRV (String.sub (Lexing.lexeme lexbuf) 1 (String.length (Lexing.lexeme lexbuf))-1)}
  | eof         { EOF }
  | _           { raise Lexical_error } 

