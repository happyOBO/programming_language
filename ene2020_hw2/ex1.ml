
type exp = X | INT of int
  | REAL of float
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  | INTEGRAL of exp * exp * exp

let rec calculate expr =
	match expr with
	| X -> raise (Failure "FreeVariable")
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1,e2) -> calculate e1 +. calculate e2
	| SUB (e1,e2) -> calculate e1 -. calculate e2
	| MUL (e1,e2) -> calculate e1 *. calculate e2
	| DIV (e1,e2) -> calculate e1 /. calculate e2
	| SIGMA (st,ed,e) -> sigma_cal (calculate st) (calculate ed) e
	| INTEGRAL (st,ed,e) -> integral_cal (calculate st) (calculate ed) e
and sigma_cal st ed e =
	if st <= ed then sigma_cal (st +. 1.) ed e +. return_cal st e
	else 0.
and integral_cal st ed e =
	if st <= (ed -. 0.1) then integral_cal (st +. 0.1) ed e +. 0.1 *. (return_cal st e)
	else 0.
and return_cal st e =
	match e with
	| X -> st
	| INT i -> float_of_int i
	| REAL f -> f
	| ADD (e1,e2) -> return_cal st e1 +. return_cal st e2
	| SUB (e1,e2) -> return_cal st e1 -. return_cal st e2
	| MUL (e1,e2) -> return_cal st e1 *. return_cal st e2
	| DIV (e1,e2) -> return_cal st e1 /. return_cal st e2
	| SIGMA (s,ed,expr) -> sigma_cal (return_cal st s) (return_cal st ed) expr
	| INTEGRAL (s,ed,expr) -> integral_cal (return_cal st s) (return_cal st ed) expr

	
(* let equals v1 v2 = (abs_float (v1 -. v2)) <= 1.0 *)

(* let test t answer =                              *)
(*   let v = calculate t in                         *)
(*   (* print_float v; print_newline (); *)         *)
(*   (equals v answer)                              *)
	

