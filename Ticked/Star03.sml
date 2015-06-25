(* Dhruv Makwana, Trinity College, dcm41 *)

(* From Ex 3 *)
datatype 'a stream = Cons of 'a * (unit -> 'a stream);

fun head (Cons(x,xf)) = x;
fun tail (Cons(x,xf)) = xf();

fun from k = Cons(k, fn () => from (k+1));
fun squares k = Cons(k*k, fn () => squares (k+1));

fun nth (s,0) = head s
  | nth (s,n) = nth (tail s, n-1);

fun mapq f (Cons(x,xf)) = Cons(f x, fn () => mapq f (xf()));

fun map2 f (Cons(x,xf)) (Cons(y,yf)) =
    Cons(f x y, fn () => map2 f (xf()) (yf()));

fun get xq 0 = []
  | get (Cons(x,xf)) n = x::get (xf()) (n-1);

(* Q1 *)
fun fib () = Cons(1, fn () =>
                 Cons(1, fn () => map2 plus (fib()) (tail (fib()) )));
nth (fib(), 14);

(* Q2 *)
fun merge (XF as Cons(x,xf)) (YF as Cons(y,yf)) =
  if x < y then Cons(x, fn () => merge (xf()) YF) else
  if x > y then Cons(y, fn () => merge XF (yf())) else
    (* x = y *) Cons(x, fn () => merge (xf()) (yf()));

get (merge (from 1) (from 2)) 15;
get (merge (squares 2) (from 5)) 8;

(* Q3 *)
fun double x = 2*x;
fun triple x = 3*x;

fun int23 () = Cons(1, fn () => merge (mapq double (int23()))
                                      (mapq triple (int23())));
get (int23()) 10;

(* Q4 *)
fun five x = 5*x;

fun int235 () = Cons(1, fn () => merge (int23()) (mapq five (int235())));
get (int23()) 15;
nth (int235(), 60);

(*
 * datatype 'a stream = Cons of 'a * (unit -> 'a stream)
 * val head = fn: 'a stream -> 'a
 * val tail = fn: 'a stream -> 'a stream
 * val from = fn: int -> int stream
 * val squares = fn: int -> int stream
 * val nth = fn: 'a stream * int -> 'a
 * val mapq = fn: ('a -> 'b) -> 'a stream -> 'b stream
 * val map2 = fn: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
 * val get = fn: 'a stream -> int -> 'a list
 * val fib = fn: unit -> int stream
 * val it = 610: int
 * val merge = fn: int stream -> int stream -> int stream
 * val it = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, ...]: int list
 * val it = [4, 5, 6, 7, 8, 9, 10, 11]: int list
 * val double = fn: int -> int
 * val triple = fn: int -> int
 * val int23 = fn: unit -> int stream
 * val it = [1, 2, 3, 4, 6, 8, 9, 12, 16, 18]: int list
 * val five = fn: int -> int
 * val int235 = fn: unit -> int stream
 * val it = [1, 2, 3, 4, 6, 8, 9, 12, 16, 18, ...]: int list
 * val it = 324: int
 *)
