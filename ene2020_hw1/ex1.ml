let rec append l1 l2 =
	match l1 with
	| [] -> l2
	| hd::tl -> hd :: append tl l2

let rec reverse l =
	match l with
	| [] -> []
	| hd::tl -> reverse tl @ [hd]


let rec rev_append l1 l2 =
	let rev_list = reverse l1 in append rev_list l2