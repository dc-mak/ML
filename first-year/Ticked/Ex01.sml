(* Dhruv Makwana, Trinity College, dcm41 *)

(* Q1 *)
fun area (x,y) = x*y/2.0;
area (3.0, 4.0);

(* Q2 *)
1.0 - 0.9 - 0.1;

(* Q3 *)
fun quadratic (a,b,c,x) = a*x*x + b*x + c : real;
fun rootplus (a,b,c) = (~b + Math.sqrt(b*b + 4.0*a*c))/(2.0*a);

val (a,b,c) = (1.0, 121.0, 11.0);
val x = rootplus (a,b,c);
quadratic(a,b,c,x);

val (a,b,c) = (1.0, 121.0, 11.0);
val x = rootplus (a,b,c);
quadratic(a,b,c,x);

(* Q4 *)
fun facr 0 = 1
  | facr n = n * facr (n-1);

fun faci n =
let
  fun fac 0 result = result
    | fac n result = fac (n-1) (n*result)
in
  fac n 1
end;

(*
 * val area = fn: real * real -> real
 * val it = 6.0: real
 * val it = ~2.775557562E~17: real
 * val quadratic = fn: real * real * real * real -> real
 * val rootplus = fn: real * real * real -> real
 * val a = 1.0: real
 * val b = 121.0: real
 * val c = 11.0: real
 * val x = 0.090840892: real
 * val it = 22.0: real
 * val a = 1.0: real
 * val b = 121.0: real
 * val c = 11.0: real
 * val x = 0.090840892: real
 * val it = 22.0: real
 * val facr = fn: int -> int
 * val faci = fn: int -> int
 * val it = (): unit
 *)
