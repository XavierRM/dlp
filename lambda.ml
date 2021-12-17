
(* TYPE DEFINITIONS *)

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

(* AUXILIAR OPERATIONS*)

let rec assoc x = function
  [] -> raise Not_found
  | (a,b,c)::l -> if compare a x = 0 then (b, c) else assoc x l
;;

(* CONTEXT MANAGEMENT *)

let emptyctx =
  []
;;

let addtbinding ctx x bind =
  (x, bind, None) :: ctx
;;

let gettbinding ctx x =
  fst (assoc x ctx)
;;

let addvbinding ctx x bind termopt =
  (x, bind, termopt) :: ctx
;;

let getvbinding ctx x =
  snd (assoc x ctx)
;;


(* TYPE MANAGEMENT (TYPING) *)
(*TyArr hace referencia a Arrow, es decir, la forma de representacion de las funciones, concretamente de los tipos de la entrada y salida de una funcion*)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyString ->
      "String"
  | TyPair (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " x " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
;;

(*El pretty printer sera relacionado a tratar de hacer que este string tenga el menor numero de parentesis, es decir, tratar de minimizar el numero de parentesis
que aparecen*)

exception Type_error of string
;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

  | TmString s1 ->
      TyString

  | TmConcat (s1, s2) -> 
      if (typeof ctx s1 = TyString) then
        if (typeof ctx s2 = TyString) then
          TyString 
        else raise (Type_error "second argument of concat is not a string")
      else raise (Type_error "first argument of concat is not a string") 

    (*T-Pair*)
  | TmPair (p1, p2) ->
      TyPair ((typeof ctx p1), (typeof ctx p2))
  
    (*T-Proj*)
  | TmProj (t1, t2, pos) ->
      if (pos == 1) then
        (typeof ctx t1)
      else
        (typeof ctx t2) 

  | TmBind (s, t) ->
      let ctx' = addvbinding ctx s (typeof ctx t) (Some t) in (typeof ctx' t) 

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try (gettbinding ctx x) with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2
  
    (*T-Fix*)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) -> 
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
          | _ -> raise (Type_error "arrow type expected"))
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
      (*Añadir las comillas*)
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      "concat (" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ ")"
  | TmPair (p1, p2) -> 
      "{" ^ string_of_term p1 ^ "," ^ string_of_term p2 ^ "}"
  | TmProj (t1, t2, pos) -> 
      "{" ^ string_of_term t1 ^ "," ^ string_of_term t2 ^ "}." ^ string_of_int pos
  | TmBind (s, t) ->
      s ^ " = " ^ string_of_term t
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmString s1 ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmPair (p1, p2) ->
      lunion (free_vars p1) (free_vars p2)
  | TmProj (t1, t2, pos) ->
      lunion (free_vars t1) (free_vars t2)
  |TmBind (s, t) ->
      free_vars t
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t -> 
      free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmString s1 ->
      TmString s1
  | TmConcat (t1, t2) ->
      TmConcat ((subst x s t1), (subst x s t2))
  | TmPair (p1, p2) ->
      TmPair (subst x s p1, subst x s p2)
  | TmProj (t1, t2, pos) ->
      TmProj (subst x s t1, subst x s t2, pos)
  | TmBind (name, t) ->
      TmBind (name, (subst x s t))
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmString _ -> true
  | TmAbs _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 t1 in
      TmLetIn (x, t1', t2) 
  
    (*E-FixBeta*)
  | TmFix (TmAbs (x, _, t12)) ->
      subst x tm t12

    (*E-Fix*)
  | TmFix t1 ->
      let t1' = eval1 t1 in
      TmFix t1'
  
    (*E-Concat*)
  | TmConcat (s1, s2) when (isval s1) && (isval s2) ->
      TmString ((string_of_term s1) ^ (string_of_term s2))

    (*E-Concat: first argument is a term*)
  | TmConcat (t1, t2) when isval t2->
      let t1' = (eval1 t1) in
      TmConcat(t1', t2)

    (*E-Concat: second argument is a term*)
  | TmConcat (t1, t2) when isval t1->
      let t2' = (eval1 t2) in
      TmConcat(t1, t2')

  | TmConcat (t1, t2) ->
      let t1' = (eval1 t1) in
      TmConcat(t1', t2)

  | TmProj(v1, v2, pos) when isval v1 && isval v2 ->
      if (pos == 1) then
        v1
      else
        v2
    (*E-Proj1*)
  | TmProj(t1, t2, pos) ->
      let t1' = (eval1 t1) in
      let t2' = (eval1 t2) in
        if (pos == 1) then
          t1'
        else
          t2'

    (*E-Pair2*)
  | TmPair (v1, t2) when isval v1 -> 
    let t2' = eval1 t2 in
      TmPair (v1, t2')

    (*E-Pair1*)
  | TmPair (t1, v2) -> 
      let t1' = eval1 t1 in
        TmPair (t1', v2)
    
  | _ ->
      raise NoRuleApplies
;;

let rec eval tm =
  try
    let tm' = eval1 tm in
    eval tm'
  with
    NoRuleApplies -> tm
;;

let execute ctx command = match command with
    Eval t -> 
              let tyTm = typeof ctx t in
              let tm = eval t in
              print_endline (string_of_term tm ^ " : " ^ string_of_ty tyTm);
              addtbinding ctx (string_of_term t) tyTm
  | Bind (s, t) -> 
              let tyTm = typeof ctx t in
              let tm = eval t in
              print_endline (string_of_term tm ^ " : " ^ string_of_ty tyTm);
              addvbinding ctx s tyTm (Some tm)
;;