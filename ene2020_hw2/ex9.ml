type relationships = string * string list
let rec is_new l e=
	match l with
	| [] -> true
	| hd::tl -> if(e = hd) then false else is_new tl e


let rec mk_l l r= 
	match l with
	| [] -> r
	| hd::tl ->  if(is_new r hd) then (mk_l tl (r@[hd])) else mk_l tl r

let uniq l = mk_l l []

let rec rem_hd (s,o) l =
	match l with
	| [] -> []
	| (s',o')::tl -> if s'=s && o'=o then (rem_hd (s,o) tl) else ([(s',o')]@ (rem_hd (s,o) tl))
	
let rec likes_tail ori gph name l = 
	match gph with
	| [] -> uniq l
	| (sub,obj)::tl ->  if sub = name && is_new l obj then 
												((let tail = likes_tail (rem_hd (sub,obj) ori) (rem_hd (sub,obj) ori) obj l in
														uniq (likes_tail (rem_hd (sub,obj) ori) tl name (l@[obj]@tail))))
											else ( likes_tail ori tl name l)

let rec all_element l result=
	match l with
	| [] -> uniq result
	| (sub,obj)::tl -> uniq (all_element tl result@[sub;obj])


let rec count_selflove g all =
	match all with
	| [] -> 0
	| hd::tl -> let likes_list = likes_tail g g hd [] in 
								(
									if not (is_new likes_list hd) then 1 + (count_selflove g tl)
									else count_selflove g tl
									)

let rec selflove g =
	let all_e_list = all_element g [] in count_selflove g all_e_list
		


(* let equals v1 v2 =       *)
(*     v1 = v2              *)

(* let test t1 answer =     *)
(*   let v = selflove t1 in *)
(*   (equals v answer)      *)