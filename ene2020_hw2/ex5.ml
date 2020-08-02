
let rec cycle_str str n =
	if n >= 0 then (cycle_str str (n-1))@[(String.get str n)]
	else []
	
let rec same l1 l2 =
	match (l1,l2) with
	| ([],[]) -> true
	| (h1::t1,h2::t2) -> if h1=h2 then same t1 t2 else false
	
let rec to_list str =
	let str_leng = String.length str in cycle_str str (str_leng-1)

let rec cut_l l ed =
	if (ed <= 0 || List.length l < ed)then []
	else(
    	match l with
    	| [] -> []
    	| hd :: tl -> hd::cut_l tl (ed-1) )

let rec cycle_cut_num ll l itv num =
	match ll with
	| [] -> num
	| hd::tl ->let cl = (cut_l ll itv) in (if cl != [] && (same cl l) then ( (cycle_cut_num tl l itv (num+1))) else (cycle_cut_num tl l itv num)
)
(* prerr_endline (string_of_int num); *)
let rec count_string str unt =
	if unt = "" then 0
	else(
	let li_str = to_list str in
		let li_unt = to_list unt in
			let unt_length = List.length li_unt in cycle_cut_num li_str li_unt unt_length 0
			)

(* let equals v1 v2 =              *)
(*     v1 = v2                     *)

(* let test t1 t2 answer =         *)
(*   let v = count_string t1 t2 in *)
(*   (equals v answer)             *)

