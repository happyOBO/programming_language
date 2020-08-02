
let rec is_new l e=
	match l with
	| [] -> true
	| hd::tl -> if(e = hd) then false else is_new tl e


let rec mk_l l r= 
	match l with
	| [] -> r
	| hd::tl ->  if(is_new r hd) then (mk_l tl (r@[hd])) else mk_l tl r

let uniq l = mk_l l []
	
(* let equals v1 v2 =   *)
(*     v1 = v2          *)

(* let test t1 answer = *)
(*   let v = uniq t1 in *)
(*   (equals v answer)  *)