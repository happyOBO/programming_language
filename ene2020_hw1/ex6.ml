type btree = Empty | Node of int * btree * btree


let rec notexists n t =
	match t with
	| Empty -> true
	| Node (k,l,r) -> (if(k=n) then false else (notexists n l) && (notexists n r))