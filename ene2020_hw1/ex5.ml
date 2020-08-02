type btree = Empty | Node of int * btree * btree

let rec height nd =
	match nd with
	| Empty -> 0
	| Node (k,l,r) -> (if (height l) > (height r) then (height l) + 1
										else (height r) + 1)
										