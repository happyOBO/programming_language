
let rec cycle_str str n =
	if n >= 0 then (cycle_str str (n-1))@[(String.get str n)]
	else []
	
let rec to_list str =
	let str_leng = String.length str in cycle_str str (str_leng-1)
		

(* let equals v1 v2 =     *)
(*     v1 = v2            *)

(* let test t answer =    *)
(*   let v = to_list t in *)
(*   (equals v answer)    *)