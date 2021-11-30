
type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
;;

type context =
  (string * ty) list
;;

(*type context =
  (string * ty * term option) list
;;

type command =
  Eval of term
  Bind of string * term;;

Si te dan Eval haces una evaluacion como antes y devuelves el mismo contexto
Si te dan un Bind entonces añades lo necesario al contexto y devuelves el mismo contexto

*)



type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
;;

val emptyctx : context;;
val addbinding : context -> string -> ty -> context;;
val getbinding : context -> string -> ty;;

(*Añadir las operaciones para coger el tipo y para coger el valor*)

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> term;;

(*val execute: tcontext * vcontext -> command -> tcontext * vcontext;;*)