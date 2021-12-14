
%{
  open Lambda;;
%}

%token LAMBDA
%token TRUE
%token FALSE
%token IF
%token THEN
%token ELSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token LETREC
%token IN
%token BOOL
%token STRING
%token CONCAT
%token NAT

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token COMMA
%token DOT
%token EQ
%token COLON
%token ARROW
%token EOF

%token <int> INTV
%token <string> STRV
%token <string> STRINGV

%start s
/*Ahora devuelve un comando en vez de un termino*/
%type <Lambda.command> s

%%


s :
    STRINGV EQ term EOF
        { Bind ($1, $3) }
    | term EOF
        { Eval $1 }


/*s :
    term EOF
      { $1 }
*/
term :
    appTerm
      { $1 }
  | IF term THEN term ELSE term
      { TmIf ($2, $4, $6) }
  | CONCAT term COMMA term
      { TmConcat ($2, $4) }
  | LBRACKET term COMMA term RBRACKET
      { TmPair ($2, $4) }
  | LBRACKET term COMMA term RBRACKET DOT STRV
      { TmProj ($2, $4, $7) }
    /*NO tiene en cuenta variables y funciones, cambiar esto a un nuevo Tm*/
  | STRINGV EQ term
      { TmBind ($1, $3) }
  | LAMBDA STRINGV COLON ty DOT term
      { TmAbs ($2, $4, $6) }
  | LET STRINGV EQ term IN term
      { TmLetIn ($2, $4, $6) }
  | LETREC STRINGV COLON ty EQ term IN term
      { TmLetIn ($2, TmFix (TmAbs ($2, $4, $6)), $8) }

appTerm :
    atomicTerm
      { $1 }
  | SUCC atomicTerm
      { TmSucc $2 }
  | PRED atomicTerm
      { TmPred $2 }
  | ISZERO atomicTerm
      { TmIsZero $2 }
  | appTerm atomicTerm
      { TmApp ($1, $2) }

atomicTerm :
    LPAREN term RPAREN
      { $2 }
  | TRUE
      { TmTrue }
  | FALSE
      { TmFalse }
  | STRINGV
      { TmVar $1 }
  | STRV
      { TmString $1 }
  | INTV
      { let rec f = function
            0 -> TmZero
          | n -> TmSucc (f (n-1))
        in f $1 }

ty :
    atomicTy
      { $1 }
  | atomicTy ARROW ty
      { TyArr ($1, $3) }

atomicTy :
    LPAREN ty RPAREN  
      { $2 } 
  | BOOL
      { TyBool }
  | NAT
      { TyNat }
  | STRING
      { TyString }

