let rec repeat str n =
	if n > 0 then str^(repeat str (n-1))
	else ""

(* let equals v1 v2 =        *)
(*     v1 = v2               *)

(* let test t1 t2 answer =   *)
(*   let v = repeat t1 t2 in *)
(*   (equals v answer)       *)