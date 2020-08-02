type program = exp
and exp = 
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | ISZERO of exp 
  | READ
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | PROC of var * exp
  | CALL of exp * exp
and var = string

exception TypeError

type typ = TyInt | TyBool 
	| TyFun of typ * typ (* t1 -> t2 *) 
	| TyVar of tyvar
and tyvar = string
type typ_eqn = (typ * typ) list (* t1 = t2 *)

let rec string_of_type ty = 
  match ty with
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1,t2) -> "(" ^ string_of_type t1 ^ " -> " ^ string_of_type t2 ^ ")"
  | TyVar x -> x

let print_typ_eqns eqns = 
  List.iter (fun (ty1,ty2) -> print_string (string_of_type ty1 ^ " = " ^ string_of_type ty2 ^ "\n")) eqns;
  print_endline ""

(* type environment : var -> type *)
module TEnv = struct
  type t = var -> typ
  let empty = fun _ -> raise (Failure "Type Env is empty")
  let extend (x,t) tenv = fun y -> if x = y then t else (tenv y)
  let find tenv x = tenv x
end

(* substitution *)
module Subst = struct
  type t = (tyvar * typ) list
  let empty = []
  let find x subst = List.assoc x subst

  (* walk through the type, replacing each type variable by its binding in the substitution *)
  let rec apply : typ -> t -> typ
  =fun typ subst ->
    match typ with
    | TyInt -> TyInt
    | TyBool -> TyBool 
    | TyFun (t1,t2) -> TyFun (apply t1 subst, apply t2 subst)
    | TyVar x -> 
      try find x subst
      with _ -> typ

  (* add a binding (tv,ty) to the substitution and propagate the information *)
  let extend tv ty subst = 
    (tv,ty) :: (List.map (fun (x,t) -> (x, apply t [(tv,ty)])) subst)

  let print : t -> unit
  =fun subst -> 
      List.iter (fun (x,ty) -> print_endline (x ^ " |-> " ^ string_of_type ty)) subst
end

let tyvar_num = ref 0

(* generate a fresh type variable *)
let fresh_tyvar () = (tyvar_num := !tyvar_num + 1; (TyVar ("t" ^ string_of_int !tyvar_num)))

let rec gen_equations : TEnv.t -> exp -> typ -> typ_eqn 
=fun tenv e ty ->
  match e with
  | READ -> [(ty,TyInt)]
  | TRUE | FALSE -> [(ty,TyBool)]
  | CONST n -> [(ty,TyInt)]
  | VAR x -> [(ty,TEnv.find tenv x)]
  | ADD (e1,e2) | SUB (e1,e2) | MUL (e1,e2) | DIV (e1,e2) -> [(ty,TyInt)]@(gen_equations tenv e1 TyInt)@(gen_equations tenv e2 TyInt)
  | ISZERO e -> [(ty,TyBool)]@(gen_equations tenv e TyInt)
  | IF (e1,e2,e3) -> (gen_equations tenv e1 TyBool)@(gen_equations tenv e2 ty)@(gen_equations tenv e3 ty)
  | LET (x,e1,e2) -> let alpha = fresh_tyvar () in (gen_equations tenv e1 alpha) @ (gen_equations (TEnv.extend (x,alpha) tenv) e2 ty)
  | LETREC (f,x,e1,e2) -> (
                            let alpha2 = fresh_tyvar () in
                              let alpha1 = fresh_tyvar () in
                                let tenv' = TEnv.extend (f,TyFun (alpha2,alpha1)) tenv in
                                (gen_equations (TEnv.extend (x,alpha2) tenv') e1 alpha1)  @ (gen_equations tenv' e2 ty) 
                          )
  | PROC (x,e) -> (
                  let alpha1 = fresh_tyvar () in
                    let alpha2 = fresh_tyvar () in
                    [(ty, TyFun (alpha1,alpha2) )] @ (gen_equations (TEnv.extend (x,alpha1) tenv) e alpha2)
                  )
  | CALL (e1,e2) -> (
                    let  alpha = fresh_tyvar () in
                    (gen_equations tenv e1 (TyFun(alpha,ty)))@(gen_equations tenv e2 alpha)
                    )          

let rec occurr_same_var x ty =
  match ty with
  | TyInt -> false
  | TyBool -> false
  | TyFun (t1,t2) -> (occurr_same_var x t1) || (occurr_same_var x t2)
  | TyVar x' -> (x' = x)

let rec refresh_subst (x,ty) sub_ty=
  match sub_ty with
  | TyInt -> TyInt
  | TyBool -> TyBool 
  | TyFun (t1,t2) -> TyFun (refresh_subst (x,ty) t1, refresh_subst (x,ty) t2)
  | TyVar x' -> if (x = x') then ty else sub_ty
(* let subst' = List.map (fun x -> Subst.apply x [(x,tv)]) subst in *)
let rec map_eqns eqns subst = 
  match eqns with
  | [] -> subst
  | (h1,h2)::tl -> (
                (* alg #1 *)
                let (tv,ty) = (Subst.apply h1 subst, Subst.apply h2 subst) in
                  if (tv != ty) then
                  (
                    match (tv,ty) with
                    | (TyInt,ty') | (TyBool,ty')-> 
                                                  (
                                                    match ty' with
                                                    | TyVar x -> 
                                                    (
                                                      let subst' = List.map (fun (x',sub_ty) -> (x',refresh_subst (x,tv) sub_ty)) subst in
                                                        (map_eqns tl (Subst.extend x tv subst'))
                                                    )
                                                    | _ -> raise TypeError
                                                  )
                    | (tv',TyInt) | (tv',TyBool)->      
                                                  (
                                                    match tv' with
                                                    | TyVar x -> 
                                                    (
                                                      let subst' = List.map (fun (x',sub_ty) -> (x',refresh_subst (x,ty) sub_ty)) subst in
                                                        (map_eqns tl (Subst.extend x ty subst'))
                                                    )
                                                    | _ -> raise TypeError
                                                  ) 
                    | (TyFun (tv1,tv2), TyFun (ty1,ty2)) -> map_eqns ([(tv1,ty1); (tv2,ty2)]@tl) subst
                    | (TyVar x,TyFun (t1,t2)) | (TyFun (t1,t2), TyVar x) ->
                                                                          (
                                                                            if ((occurr_same_var x t1) || (occurr_same_var x t2))then raise TypeError
                                                                            else 
                                                                              (
                                                                                let subst' = List.map (fun (x',sub_ty) -> (x',refresh_subst (x,(TyFun (t1,t2))) sub_ty)) subst in
                                                                                  (map_eqns tl (Subst.extend x (TyFun (t1,t2)) subst'))
                                                                              )
                                                                          )
                    | (TyVar x, TyVar y) -> 
                                            (
                                              let subst' = List.map (fun (x',sub_ty) -> (x',refresh_subst (x,TyVar y) sub_ty)) subst in
                                                (map_eqns tl (Subst.extend x (TyVar y) subst'))
                                            )
                  )
                else map_eqns tl subst
              )
let solve : typ_eqn -> Subst.t
=fun eqns -> map_eqns eqns Subst.empty



(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp ->
  let new_tv = fresh_tyvar () in
  let eqns = gen_equations TEnv.empty exp new_tv in
  let _ = print_endline "= Equations = ";
          print_typ_eqns eqns in
  try 
    let subst = solve eqns in
    let ty = Subst.apply new_tv subst in
     print_endline "= Substitution = ";
      Subst.print subst;
      print_endline "";
      print_endline ("Type of the given program: " ^ string_of_type ty);
      print_endline "";
      ty
  with TypeError -> (print_endline "The program does not have type. Rejected."); exit (1)