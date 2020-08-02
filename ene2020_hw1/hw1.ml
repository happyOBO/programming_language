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

let rec range i1 i2 =
	if i1 > i2 then []
	else i1 :: range (i1+1) i2

let rec tail_part p l l1 l2 =
	match l with
	| [] -> (l1,l2)
	| hd::tl -> (if(p hd) then tail_part p tl (l1@[hd]) l2 
							else tail_part p tl l1 (l2@[hd]))


let partition p l = tail_part p l [] []

 type formula = TRUE | FALSE
    | NOT of formula
    | ANDALSO of formula * formula
    | ORELSE of formula * formula
    | IMPLY of formula * formula
    | LESS of expr * expr
  and expr = NUM of int
    | PLUS of expr * expr
    | MINUS of expr * expr

let rec eval_expr exp =
	match exp with
	| NUM k -> k
	| PLUS (e1,e2) -> (eval_expr e1) + (eval_expr e2)
	| MINUS (e1,e2) -> (eval_expr e1) - (eval_expr e2)
		
let rec eval fma =
	match fma with
	| TRUE -> true
	| FALSE -> false
	| NOT f -> not (eval f)
	| ANDALSO (f1,f2) -> (eval f1) && (eval f2)
	| ORELSE (f1,f2) -> (eval f1) || (eval f2)
	| IMPLY (f1,f2) -> (not (eval f1)) || (eval f2)
	| LESS (e1,e2) -> (eval_expr e1) < (eval_expr e2)


type btree = Empty | Node of int * btree * btree

let rec height nd =
	match nd with
	| Empty -> 0
	| Node (k,l,r) -> (if (height l) > (height r) then (height l) + 1
										else (height r) + 1)
										

let rec notexists n t =
	match t with
	| Empty -> true
	| Node (k,l,r) -> (if(k=n) then false else (notexists n l) && (notexists n r))
	
	
let rec fold3 f a l1 l2 l3 =
	match (l1,l2,l3) with
	| ([],_,_) -> a
	| (h1::t1,h2::t2,h3::t3) -> (fold3 f (f a h1 h2 h3) t1 t2 t3)



let compose f g = fun x -> f(g(x))

let rec iter n f= 
  match n with
  | 0 -> fun x -> x
  | _ -> compose f (iter (n-1) f)
	
let rec sigma (a ,b ,f) =
	if (a > b) then 0
	else ((fun x -> f x) a) + sigma ((a+1), b, f)
	
	
let rec map f l =
  match l with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)
	
let rec cartesian a b =
	match (a,b) with
	| ([],_) | (_,[]) -> []
	| (h::t,_) -> (map (fun x -> (h,x)) b) @ cartesian t b
	

