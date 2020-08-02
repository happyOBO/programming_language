let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)
	
let rec cartesian a b =
	match (a,b) with
	| ([],_) | (_,[]) -> []
	| (h::t,_) -> (map (fun x -> (h,x)) b) @ cartesian t b