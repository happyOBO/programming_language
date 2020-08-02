type program = exp
and exp = 
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
  | NEWREF of exp 
  | DEREF of exp
  | SETREF of exp * exp
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int 
  | Bool of bool 
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | Loc of loc
and loc = int
and env = (var * value) list
and mem = (loc * value) list

(* conversion of value to string *)
let value2str v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Loc l -> "Loc "^(string_of_int l)
  | Procedure (x,e,env) -> "Procedure "
  | RecProcedure (f,x,e,env) -> "RecProcedure "^f

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::m
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int l ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

(* use the function 'new_location' to generate a fresh memory location *)
let counter = ref 0
let new_location () = counter:=!counter+1;!counter

exception NotImplemented
exception UndefinedSemantics
(*****************************************************************)
(* TODO: Implement the eval function. Modify this function only. *)
(*****************************************************************)

let rec eval : exp -> env -> mem -> value * mem
=fun exp env mem -> 
match exp with
| CONST n -> (Int n,mem)
| VAR x -> (apply_env env x,mem)
| LET (x,e1,e2) -> (let (v1,mem1) = eval e1 env mem in (eval e2 (extend_env (x,v1) env) mem1))
| PROC (x,e) -> (Procedure (x,e,env),mem)
| CALL (e1,e2) -> (
                    let (value,mem1) = eval e1 env mem in
                      (match value with
                      | Procedure (x,e,env') -> (
                                                  let (v,mem2) = eval e2 env mem1 in 
                                                    (eval e (extend_env (x,v) env') mem2)
                                                )
                      | RecProcedure (f,x,e,env') -> (
                                                      let (v,mem2) = eval e2 env mem1 in 
                                                        (eval e (extend_env (f,RecProcedure(f,x,e,env')) (extend_env (x,v) env')) mem2)
                                                    )
                      )

                  )
| ADD (e1,e2) -> (
                    let (Int n1,mem1) = eval e1 env mem in
                      let (Int n2,mem2) = eval e2 env mem1 in
                        (Int (n1 + n2) , mem2) 
                  )
| SUB (e1,e2) ->  (
                    let (Int n1,mem1) = eval e1 env mem in
                      let (Int n2,mem2) = eval e2 env mem1 in
                        (Int (n1 - n2) , mem2) 
                  )
| MUL (e1,e2) ->  (
                    let (Int n1,mem1) = eval e1 env mem in
                      let (Int n2,mem2) = eval e2 env mem1 in
                        (Int (n1 * n2) , mem2) 
                  )
| DIV (e1,e2) ->  (
                    let (Int n1,mem1) = eval e1 env mem in
                      let (Int n2,mem2) = eval e2 env mem1 in
                        (Int (n1 / n2) , mem2) 
                  )
| ISZERO e -> (
                let (Int n,mem1) = eval e env mem in
                  (if n = 0 then (Bool true, mem1) else (Bool false, mem1))
              )
| READ -> let n = read_int() in (Int n,mem)
| IF (e1,e2,e3) -> (
                      let (Bool b,mem1) = eval e1 env mem in
                        (if b = true then (eval e2 env mem1) else (eval e3 env mem1))
                    ) 
| LETREC (f,x,e1,e2) -> eval e2 (extend_env (f,RecProcedure(f,x,e1,env)) env) mem
| NEWREF e -> (
                let (v,mem1) = eval e env mem in
                  let l = new_location () in
                    (Loc l,(extend_mem (l,v) mem1))
              )
| DEREF e -> (
                let (Loc l, mem1) = eval e env mem in
                  (apply_mem mem1 l,mem1)
              )
| SETREF (e1,e2) -> (
                      let (Loc l, mem1) = eval e1 env mem in
                        let (v,mem2) = eval e2 env mem1 in
                          (v,(extend_mem (l,v) mem2))
                    )
| SEQ (e1,e2) -> (
                    let (v1,mem1) = eval e1 env mem in
                      let (v2,mem2) = eval e2 env mem1 in
                        (v2,mem2)
                )
| BEGIN e -> (
                let (v,mem1) = eval e env mem in
                  (v,mem1)
            )
| _ -> raise UndefinedSemantics


(* driver code *)
let run : program -> value
=fun pgm -> (fun (v,_) -> v) (eval pgm empty_env empty_mem) 
