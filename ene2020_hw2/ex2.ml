
type ae = CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list
(* let (|>) g f = f g                                             *)
(* type r = (int * ((string * int) list)) list                    *)

(* let rec sum r1 r2 =                                            *)
(*     match r1, r2 with                                          *)
(*     | _, [] -> r1                                              *)
(*     | [], _ -> r2                                              *)
(*     | (c1, xs1)::t1, (c2, xs2)::t2 ->                          *)
(*         if xs1 = xs2 then (c1 + c2, xs1)::(sum t1 t2)          *)
(*         else if xs1 < xs2 then                                 *)
(*             (c1, xs1)::(sum t1 r2)                             *)
(*         else                                                   *)
(*             (c2, xs2)::(sum r1 t2)                             *)
(* let rec mult res a =                                           *)

(*     match a with                                               *)
(*     | CONST c1 ->                                              *)
(*         List.map (fun (c, xs) -> (c1 * c, xs)) res             *)

(*     | VAR v ->                                                 *)
(*         mult res (POWER (v, 1))                                *)

(*     | POWER (x1, n1) ->                                        *)
(*         let r =                                                *)
(*             List.map (fun (c, xs) ->                           *)
(*                 let rec iter rlst =                            *)
(*                     match rlst with                            *)
(*                     | [] -> [(x1, n1)]                         *)
(*                     | (x2, n2)::tl ->                          *)
(*                         if x1 = x2 then (x2, n1 + n2)::tl      *)
(*                         else if x1 < x2 then (x1, n1)::rlst    *)
(*                         else (x2, n2)::(iter tl) in            *)
(*                 (c, iter xs)) res in                           *)
(*         List.fold_left (fun res elem -> sum res [elem]) [] r   *)

(*     | SUM alst -> (                                            *)
(*         match alst with                                        *)
(*         | [] -> []                                             *)
(*         | a::tl -> sum (mult res a) (mult res (SUM tl))        *)
(*     )                                                          *)

(*     | TIMES alst ->                                            *)
(*     (                                                          *)
(*         match alst with                                        *)
(*         | [] -> res                                            *)
(*         | a::tl -> mult (mult res a) (TIMES tl)                *)
(*     )                                                          *)


(* let normalize a =                                              *)
(*     (mult [1, []] a)                                           *)
(*     |> List.filter (fun (c, _) -> c <> 0)                      *)


(* let equals n1 a =                                              *)
(*     n1 = (normalize a)                                         *)


let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)


let rec find l d num =
	match l with
	| [] -> -1
	| hd :: tl -> if hello hd d num != -1 then num else find tl d (num+1)
and hello hd d num =
	match hd with
	| POWER (v,exp) -> if v = d then num else -1
	| SUM li -> if find li d num != -1 then num else -1
	| TIMES li -> if find li d num != -1 then num else -1
	| VAR v -> if v = d then num else -1
	| CONST c -> -1

and nth_diff equat d n =
	if n < 0 then equat
	else 
	(match equat with
	| [] -> []
	| hd::tl -> (if n = 0 then  (diff (hd,d))::tl else hd::(nth_diff tl d (n-1)))
	)
and diff (equat,d)=
	match equat with
	| VAR v -> if v = d then CONST 1 else CONST 0
	| CONST c -> CONST 0
	| POWER (v,exp) -> if v = d then TIMES [ CONST exp ; POWER (v,exp-1)] else CONST 0
	| SUM l -> let cl = map (fun x -> diff (x,d) ) l in SUM cl
	| TIMES l -> SUM (sum_times [] l d)

and sum_times left l d=
	match l with
	| [] -> []
	| hd::tl -> [TIMES (diff(hd,d)::left@tl )] @ (sum_times (left@[hd]) tl d)





