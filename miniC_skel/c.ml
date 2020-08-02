type program = exp
and exp =
	| SKIP
	| TRUE
	| FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
	| LE of exp * exp
	| EQ of exp * exp
	| NOT of exp 
  | IF of exp * exp * exp
	| WHILE of exp * exp 
	| LET of var * exp * exp
	| PROC of var list * exp 
	| CALLV of exp * exp list 
	| CALLR of exp * var list
	| ASSIGN of var * exp 
	| RECORD of (var * exp) list 
	| FIELD of exp * var
	| ASSIGNF of exp * var * exp 
  | READ of var
	| PRINT of exp 
  | SEQ of exp * exp
  | BEGIN of exp
and var = string

type value = 
    Int of int
  | Bool of bool
	| Unit
  | Procedure of var list * exp * env
	| Record of record
  | Loc of loc
and loc = int 
and env = (var * loc) list
and mem = (loc * value) list
and record = (var * loc) list

(* conversion of value to string *)
let value2str v =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Unit -> "."  
	| Procedure (params,e,env) -> "Procedure "
  | Record record -> "Record "
	| Loc l -> "Loc "^(string_of_int l)

(* environment *)
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env e x = 
  match e with
  | [] -> raise (Failure (x ^ " is unbound in Env"))
  | (y,v)::tl -> if x = y then v else apply_env tl x

(* memory *)
let empty_mem = [] 
let extend_mem (l,v) m = (l,v)::(List.filter (fun (l',_) -> l != l') m)
let rec apply_mem m l = 
  match m with
  | [] -> raise (Failure ("Location " ^ string_of_int (l) ^ " is unbound in Mem"))
  | (y,v)::tl -> if l = y then v else apply_mem tl l

let counter = ref 0
let new_location () = counter:=!counter+1; (!counter)

(* conversion of env to string *)
let string_of_env env = 
	List.fold_left (fun str (x,l) -> Printf.sprintf "%s\n%s -> %d" str x l) "" env  
(* conversion of mem to string *)
let string_of_mem mem = 
	List.fold_left (fun str (l,v) -> Printf.sprintf "%s\n%d -> %s" str l (value2str v)) "" mem 		
		
exception NotImplemented
exception UndefinedSemantics
(* if the following variable is set true, gc will work (otherwise, gc simply returns a given memory). *)
let remove_garbage = ref false 


let rec is_new l e=
	match l with
	| [] -> true
	| hd::tl -> if(e = hd) then false else is_new tl e


let rec mk_l l r= 
	match l with
	| [] -> r
	| hd::tl ->  if(is_new r hd) then (mk_l tl (r@[hd])) else mk_l tl r

let uniq l = mk_l l []

(*reach_l reach reach mem reach*)

let rec reach env mem =
	let env_x = List.map (fun (x,l) -> l) env in
		uniq ((reach_l env_x mem) @ env_x)
and reach_l loc mem =
	match loc with
	| [] -> []
	| hd::tl ->
	let ml = apply_mem mem hd in
		(
			match ml with
			| Loc l ->	(reach_l tl mem)@[l]
			| Record l -> 
			(
				let nloc = List.map (fun (x,l') -> l') l in
					(reach_l tl mem) @ nloc
			)
			| Procedure (x,e,env') -> (reach_l tl mem) @(reach env' mem)
			| _ -> reach_l tl mem
		)
		
		 

let gc: env * mem -> mem
= fun (env, mem) ->
	if (not !remove_garbage) then mem 
	else 
	(
		let rem = reach env mem in
			List.map (fun x -> (x , apply_mem mem x) ) rem
	)
		

let rec apply_rec r x= 
	match r with
	| [] -> raise (Failure ("Record is unbounded"))
	| (y,v)::tl -> if x = y then v else apply_rec tl x


let rec eval : program -> env -> mem -> (value * mem)
=fun pgm env mem ->  
  match pgm with
  | READ x -> (Unit, extend_mem (apply_env env x, Int (read_int())) mem) (* Do not modify *)
	| PRINT e ->
		let v, mem' = eval e env mem in
		let _ = print_endline (value2str v) in
		(v, gc(env,mem')) (* Do not modify *) 
	| CONST n -> (Int n,mem)
	| TRUE -> (Bool true,mem)
	| FALSE -> (Bool false,mem)
	| SKIP -> (Unit, mem)
	| VAR x -> (apply_mem mem (apply_env env x),mem)
	| ADD (e1,e2) -> (
                    let (n1,mem1) = eval e1 env mem in
											let (n2,mem2) = eval e2 env mem1 in
											(
											match (n1,n2) with
											| (Int n1' ,Int n2')-> (Int (n1' + n2') , mem2) 
											| _ ->  raise UndefinedSemantics
											)
                  )
	| SUB (e1,e2) ->  (
											let (n1,mem1) = eval e1 env mem in
												let (n2,mem2) = eval e2 env mem1 in
												(
													match (n1,n2) with
													| (Int n1' ,Int n2')-> (Int (n1' - n2') , mem2) 
													| _ ->  raise UndefinedSemantics
													) 
										)
	| MUL (e1,e2) ->  (
											let (n1,mem1) = eval e1 env mem in
												let (n2,mem2) = eval e2 env mem1 in
												(
													match (n1,n2) with
													| (Int n1' ,Int n2')-> (Int (n1' * n2') , mem2) 
													| _ ->  raise UndefinedSemantics
													)
										)
	| DIV (e1,e2) ->  (
											let (n1,mem1) = eval e1 env mem in
												let (n2,mem2) = eval e2 env mem1 in
												(
													match (n1,n2) with
													| (Int n1' ,Int n2')-> (Int (n1' / n2') , mem2) 
													| _ ->  raise UndefinedSemantics
													)
										)
	| LET (x,e1,e2) -> (
		let (v1, mem1) = eval e1 env mem in
			let l = new_location() in
				eval e2 (extend_env (x,l) env) (extend_mem (l,v1) mem1)
		)
	| LE (e1,e2) -> (

										let (n1,mem1) = eval e1 env mem in
											let (n2,mem2) = eval e2 env mem1 in
												(
													match (n1,n2) with
													| (Int n1' ,Int n2') -> if (n1' <= n2') then (Bool true,mem2) else (Bool false,mem2)
													| _ -> raise UndefinedSemantics
												)
									)
	| EQ (e1,e2) -> (
										let (n1,mem1) = eval e1 env mem in
											let (n2,mem2) = eval e2 env mem1 in
												(
													match (n1,n2) with
													| (Int n1',Int n2') -> (Bool (n1' = n2') ,mem2)
													| (Bool n1',Bool n2') -> (Bool (n1' = n2') ,mem2)
													| (Unit,Unit) -> (Bool true, mem2)
													| _ -> (Bool false, mem2)
												)
									)
	| NOT e -> (
								let (b, mem1) = eval e env mem in 
								match b with
								| Bool b' ->(Bool (not b'),mem1)
								| _ -> raise UndefinedSemantics
								
							)
	| IF (e1,e2,e3) -> (
		let (b,mem1) = eval e1 env mem in
		match b with
		| Bool b' -> (if b' = true then (eval e2 env mem1) else (eval e3 env mem1))
		| _ -> raise UndefinedSemantics
	)
	| WHILE (e1,e2) -> (
												let (b,mem0) = eval e1 env mem in
												match b with
												| Bool b' ->
													(
														if b' then
														(
															let (v1,mem1) = eval e2 env mem0 in
																eval (WHILE (e1,e2)) env mem1
														)
														else (Unit,mem0)
													)
													| _ -> raise UndefinedSemantics
											)
	| PROC (xlist,e) -> (Procedure (xlist,e,env),mem)
	| ASSIGN (x, e) -> let (v,mem1) = eval e env mem in
												(v, 
												extend_mem ((apply_env env x),v) mem1
												)
	| CALLV (e0,el)-> let (proc_v,mem0) = eval e0 env mem in
											(
												match proc_v with
												| Procedure (xl,e,env') ->(
																										let (mem_n,vl) =  ret_vlist el env mem0 [] in
																											let (add_xl_env,add_vl_mem) = add_list xl vl env' mem_n in
																												eval e add_xl_env add_vl_mem
																									)
												| _ -> raise UndefinedSemantics
											)
	| CALLR (e0,yl)-> let (proc_v,mem0) = eval e0 env mem in
											(
												match proc_v with
												| Procedure (xl,e,env') ->
												(
													let xtoenv = List.map2 (fun x y -> (x,apply_env env y) ) xl yl in
														let env'' = append_list xtoenv env' in
															eval e env'' mem0
												)
												| _ ->raise UndefinedSemantics
											)
	| RECORD lst -> if lst = [] then (Unit,mem)
									else
									(
										let (xl,vl,mem_n) =  eval_rec lst env mem [] [] in
											let (r,m) = add_list xl vl [] mem_n in
												(Record r,m)
									)
	| FIELD (e,x) -> let (r,mem1) = eval e env mem in
											(match r with
											| Record r' ->
																	(
																		let rx = apply_rec r' x in
																			((apply_mem mem1 rx),mem1)
																	)
											| _ -> raise UndefinedSemantics)
		| ASSIGNF (e1,x,e2) ->	let (r,mem1) = eval e1 env mem in
															(match r with
															| Record r' ->
																					(
																						let (v,mem2) = eval e2 env mem1 in
																							let rx = apply_rec r' x in
																								(v, (extend_mem (rx,v) mem2) )
																					)
															| _ -> raise UndefinedSemantics)
	| SEQ (e1,e2) -> let (v1,mem1) = eval e1 env mem in
										eval e2 env mem1
	| BEGIN e -> (
								let (v,mem1) = eval e env mem in
									(v,mem1)
								)
and eval_rec l env mem xl vl =
	match l with
	| [] -> (xl,vl,mem)
	| (x,e)::tl -> let (v,mem') = eval e env mem in
										eval_rec tl env mem' (xl@[x]) (vl@[v])
and append_list l1 l2 =
	match l1 with
	| [] -> l2
	| hd::tl -> append_list tl (l2@[hd])
and add_list xl vl env mem =
	match (xl,vl) with
	| ([],[]) -> (env,mem)
	| (h1::t1, h2::t2) -> (
													let l = new_location () in 
														add_list t1 t2 (extend_env (h1,l) env) (extend_mem (l,h2) mem)
												)
	| _ -> raise UndefinedSemantics
and ret_vlist el env mem addv=
	match el with
	| [] -> (mem, addv)
	| hd::tl -> (
								let (v',mem') = eval hd env mem in
											let addv' = addv@[v'] in
												ret_vlist tl env mem' addv'
							)

let run : program -> bool -> bool -> unit 
= fun pgm with_gc print_mem_size ->
	let _ = remove_garbage := with_gc in 
	let mem = snd (eval pgm empty_env empty_mem) in   
	if (print_mem_size) then 
		print_endline (Printf.sprintf "Final mem size: %d" (List.length mem))
	
	
