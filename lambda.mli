
type ty =
    TyBool
  | TyNat
  | TyString
  | TyPair of ty * ty
  | TyArr of ty * ty
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmString of string
  | TmConcat of term * term
  | TmPair of term * term
  | TmProj of term * term * int
  | TmBind of string * term
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
;;

type context =
  (string * ty * term option) list
;;

type command =
    Eval of term
  | Bind of string * term;;


val emptyctx : context;;
val addtbinding : context -> string -> ty -> context;;
val gettbinding : context -> string -> ty;;
val addvbinding : context -> string -> ty -> term option -> context;;
val getvbinding : context -> string -> term option;;

val string_of_ty : ty -> string;;
exception Type_error of string;;
val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : term -> context -> term;;

val execute: context -> command -> context;;