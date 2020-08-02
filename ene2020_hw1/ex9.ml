	
let rec sigma (a ,b ,f) =
	if (a > b) then 0
	else ((fun x -> f x) a) + sigma ((a+1), b, f)
	