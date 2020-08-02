let rec rev_append l1 l2 =
	match l2 with
	| [] -> l2
	| hd :: tl -> (rev_append tl [] ) @ [hd] @ l2

let rec range lower upper =
	if lower > upper then []
	else lower :: (range (lower + 1) upper)
	
let rec partition p l =
	match l with
	| [] -> ([],[])
	| hd :: tl ->
		let (ts, fs) = partition p tl in
		if (p hd) then (hd:: ts,fs)
		else (ts,hd::fs)
		
type formula = TRUE | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
  and expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

let rec calc exp =
	match exp with
	| NUM n -> n
	| PLUS (exp1,exp2) -> (calc exp1) + (calc exp2)
	| MINUS (exp1, exp2) -> (calc exp1) - (calc exp2)

let rec eval form =
	match form with
	| TRUE -> true
	| FALSE -> false
	| NOT form' -> not (eval form')
	| ANDALSO (form1, form2) -> (eval form1) && (eval form2)
	| ORELSE (form1, form2) -> (eval form1) || (eval form2)
	| IMPLY (form1, form2) -> (not (eval form1) ) || (eval form2)
	| LESS (expr1, expr2) -> (calc expr1) < (calc expr2)

type btree = Empty | Node of int * btree * btree

let rec height t =
	match t with
	| Empty -> 0
	| Node (n, l , r) -> (if (height l) > (height r) then (height l) + 1
										else (height r) + 1)
										
let rec notexists n t =
	match t with
	| Empty -> true
	| Node (n',l,r) -> 
		if(n = n') then false
		else (notexists n l) && (notexists n r)

let rec fold3 f a blst clst dlst =
	match blst, clst, dlst with
	| hd1:: tl1, hd2::tl2, hd3::tl3 -> 
		fold3 f (f a hd1 hd2 hd3) tl1 tl2 tl3
	|[],[],[] -> a
	| _ -> raise (Failure "fail")

let rec iter n f = 
	if n = 0 then (fun x -> x)
	else 
		(fun x -> f (iter (n-1) f))
		
let rec sigma (a,b,f) =
	if a > b then 0
	else 
		(f a) + sigma (a+1, b, f) 
		
let rec cartesian alst blst =
	match alst with
	| [] -> []
	| hd::[] -> map (fun b -> (hd,b))blst
	| hd:: tl -> (cartesian [hd] blst) @ (cartesian tl blst)
		
	
	