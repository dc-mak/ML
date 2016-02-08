(* Dhruv Makwana, Trinity College, dcm41 *)

(* Q1 *)
fun prod m 0 = 0
  | prod m n = m + prod m (n-1);

fun pow m 0 = 1
  | pow m n = m * pow m (n-1);

prod 2 3;
pow 2 3;

(* Q2 *)
fun nfold (f,0) = (fn x => x)
  | nfold (f,n) = fn x => f (nfold (f,n-1) x);

fun increment x = x + 1;

fun plus x y = nfold (increment, y) x;

fun times x y = nfold (plus x, y) 0;

fun toPow x y = nfold (times x, y) 1;

plus 7 8;
times 4 9;
toPow 2 4;

(* Q2 *)
datatype 'a stream = Cons of 'a * (unit -> 'a stream);
fun from k = Cons(k, fn () => from (k+1));

fun head (Cons(x,xf)) = x;
fun tail (Cons(x,xf)) = xf();

(* Q3 *)
fun nth (s,0) = head s
  | nth (s,n) = nth (tail s, n-1);

(* Q4 *)
fun squares k = Cons(k*k, fn () => squares (k+1));

val sq = squares 0;
nth (sq, 3);
nth (sq, 2);
nth (sq, 1);
nth (sq, 48);

(* Q5 *)
fun map2 f (Cons(x,xf)) (Cons(y,yf)) =
    Cons(f x y, fn () => map2 f (xf()) (yf()));

(*
 * val prod = fn : int -> int -> int
 * val pow = fn: int -> int -> int
 * val it = 6: int
 * val it = 8: int
 * val nfold = fn: ('a -> 'a) * int -> 'a -> 'a
 * val increment = fn: int -> int
 * val plus = fn: int -> int -> int
 * val times = fn: int -> int -> int
 * val toPow = fn: int -> int -> int
 * val it = 15: int
 * val it = 36: int
 * val it = 16: int
 * datatype 'a stream = Cons of 'a * (unit -> 'a stream)
 * val from = fn: int -> int stream
 * val head = fn: 'a stream -> 'a
 * val tail = fn: 'a stream -> 'a stream
 * val nth = fn: 'a stream * int -> 'a
 * val squares = fn: int -> int stream
 * val sq = Cons (0, fn): int stream
 * val it = 9: int
 * val it = 4: int
 * val it = 1: int
 * val it = 2304: int
 * val map2 = fn: ('a -> 'b -> 'c) -> 'a stream -> 'b stream -> 'c stream
 *)
