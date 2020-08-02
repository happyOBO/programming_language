let rec tail_part p l l1 l2 =
	match l with
	| [] -> (l1,l2)
	| hd::tl -> (if(p hd) then tail_part p tl (l1@[hd]) l2 
							else tail_part p tl l1 (l2@[hd]))


let partition p l = tail_part p l [] []
