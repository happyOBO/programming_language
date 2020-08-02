open M

(* exp environment : var -> exp *)

module EEnv = struct
  type t = var -> exp
  let empty = fun _ -> raise (Failure "Exp Env is empty")
  let extend (x,t) eenv = fun y -> if x = y then t else (eenv y)
  let find eenv x = eenv x
end

let rec used x e =
  match e with
  | TRUE -> false
  | FALSE -> false
  | CONST n -> false
  | VAR x' -> x = x'
  | ADD (e1, e2) -> (used x e1) || (used x e2)
  | SUB (e1, e2) -> (used x e1) || (used x e2)
  | MUL (e1, e2) -> (used x e1) || (used x e2)
  | DIV (e1, e2) -> (used x e1) || (used x e2)
  | ISZERO e -> used x e
  | READ -> false
  | IF (e1,e2,e3)-> (used x e1 ) || (used x e2 ) || (used x e3)
  | LET (x,e1,e2) -> (used x e1) || (used x e2)
  | LETREC (f,x,e1,e2) -> (used x e1) || (used x e2)
  | PROC (x,e) -> used x e
  | CALL (e1,e2) -> (used x e1) || (used x e2)

let rec env_expand exp env =
  match exp with
  | TRUE -> TRUE
  | FALSE -> FALSE
  | CONST n -> CONST n
  | VAR x -> (try EEnv.find env x with _ -> exp)
  | ADD (e1, e2) -> ADD (env_expand e1 env, env_expand e2 env)
  | SUB (e1, e2) -> SUB (env_expand e1 env, env_expand e2 env)
  | MUL (e1, e2) -> MUL (env_expand e1 env, env_expand e2 env)
  | DIV (e1, e2) -> DIV (env_expand e1 env, env_expand e2 env)
  | ISZERO e -> ISZERO (env_expand e env)
  | READ -> READ
  | IF (e1,e2,e3)-> IF (env_expand e1 env, env_expand e2 env, env_expand e3 env)
  | LET (x,e1,e2) -> 
                    (
                      if used x e2 then env_expand e2 (EEnv.extend (x,env_expand e1 env) env)
                      else LET(x,env_expand e1 env, env_expand e2 env)
                    )
  | LETREC (f,x,e1,e2) -> LETREC (f,x,env_expand e1 env,env_expand e2 env)
  | PROC (x,e) -> PROC (x,env_expand e env)
  | CALL (e1,e2) -> CALL (env_expand e1 env, env_expand e2 env)

let expand: exp -> exp 
= fun exp -> env_expand exp EEnv.empty


(* typeof: Do not modify this function *)
let typeof : exp -> typ 
=fun exp -> 
	let exp' = expand exp in 
	M.typeof exp'  
