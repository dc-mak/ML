(*1993 Paper 1 Question 5
-------------------------
Lazy Lists
------------------------- *)
datatype 'a seq = Nil | Cons of unit -> 'a*'a seq;

fun infinite n = Cons (fn() => (n, infinite(n+1)));

exception Empty;
fun hd (Cons(x)) = #1(x())
  | hd Nil = raise Empty;

fun tl (Cons(x)) = #2(x())
  | tl Nil = raise Empty;

fun first (Cons(x)) = x()
  | first Nil = raise Empty;

fun next(_, Cons(x)) = x()
  | next (_, Nil) = raise Empty;

exception Butlast;
fun butlast Nil = raise Butlast
  | butlast list = first(list);
