fun sum [] us = us : (int * real) list
  | sum ts [] = ts
  | sum ((m,a)::ts) ((n,b)::us) =
  if m>n then ((m,a)::(sum ts ((n,b)::us))) else
  if m<n then ((n,b)::(sum ((m,a)::ts) us))
  else if Real.==(a+b, 0.0) then
    sum ts us else 
    ((m, a+b)::(sum ts us));
    
fun termprod (m, a) (n, b) = (m+n, a*b:real);

fun map f [] = [] | map f (x::xs) = (f x)::(map f xs);

fun take([], _) = []
  | take(x::xs, n) = if n > 0 then x::take(xs, n-1) else [];

fun drop([], _) = []
  | drop(x::xs, n) = if n>0 then drop(xs, n-1) else x::xs;

fun prod [] us = []
  | prod [(m,a)] us = map (termprod(m,a)) us
  | prod ts us =
  let
    val k = length ts div 2
  in
    sum (prod (take(ts,k)) us) (prod (drop(ts,k)) us)
  end;

fun quorem ts ((n, b)::us) =
  let 
    fun quo [] qs = (rev qs, [])
      | quo ((m,a)::ts) qs =
          if m<n then (rev qs, (m, a)::ts)  
          else quo (sum ts (map (termprod(m-n, ~a/b)) us)) 
                   ((m-n, a/b)::qs)
  in
    quo ts []
  end;
