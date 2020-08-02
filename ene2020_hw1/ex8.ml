let compose f g = fun x -> f(g(x))

let rec iter n f= 
  match n with
  | 0 -> fun x -> x
  | _ -> compose f (iter (n-1) f)