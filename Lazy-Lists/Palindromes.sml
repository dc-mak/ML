datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);

fun get _ Nil = [] | get 0 _ = []
  | get n (Cons(x,xf)) = x::(get (n-1) (xf()));

fun map f Nil = Nil
  | map f (Cons(x,xf)) = Cons(f x, fn()=> map f (xf()));

fun inter xq Nil = xq
  | inter (Cons(x,xf)) yf = Cons(x, fn()=> inter yf (xf()));

fun add n x = n::x;

fun padd n [] = [n] | padd n x = n::rev(n::x);

fun pzo() = Cons([], fn()=> inter (map (padd 0) (pzo()))
                                  (map (padd 1) (pzo())));
