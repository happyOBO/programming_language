type btree = Empty | Node of int * btree * btree

let rec height nd =
	match nd with
	| Empty -> 0
	| Node (k,l,r) -> (if (height l) > (height r) then (height l) + 1
										else (height r) + 1)


let rec check btr =
	match btr with
  | Empty -> true
	| Node (k,l,r) -> let gap = abs((height l) - (height r)) in 
										(if gap > 1 then false
											else (check l && check r))
											


(* let equals v1 v2 =    *)
(*     v1 = v2           *)

(* let test t1 answer =  *)
(*   let v = check t1 in *)
(*   (equals v answer)   *)