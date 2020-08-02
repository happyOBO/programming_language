(* type relationships = string * string list *)

let rec is_new l e=
	match l with
	| [] -> true
	| hd::tl -> if(e = hd) then false else is_new tl e


let rec mk_l l r= 
	match l with
	| [] -> r
	| hd::tl ->  if(is_new r hd) then (mk_l tl (r@[hd])) else mk_l tl r

let uniq l = mk_l l []

(* let rec likes_also gph name l = *)
(* 	match gph                     *)

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


let rec likes gph name = List.length (likes_tail gph gph name [])


let equals v1 v2 =
    v1 = v2

let test t1 t2 answer =
  let v = likes t1 t2 in
  (equals v answer)

let r1 : (string * string) list = [("A","B");("B","A");("B","E");("D","B");("D","E");("F","F");("A","G");("G","B");("A","E");("E","E");]
