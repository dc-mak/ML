(* 2.1: poly, use "Chap2.sml"; *)

(* 2.2:
 * Good (correction):
 * Eliminates overloaded operators and fewer concepts to rememmber.
 *
 * Bad:
 * Although this could work for 32b ints and 64b floats, the distinction
 * is there for many reasons: real numbers cannot be represented exactly,
 * equality becomes tougher with floats, loss of precision or even a
 * computability problem about when to terminate a floating point
 * calculation. *)

(* 2.3: last one *)

(* 2.4:
 * First one: ~1 -> / and 10 -> :
 * Second one: some sort of exception (Subscript ?) raised. *)

(* 2.5: *)
type date = int * string;
fun check_date ((d, m) : date) =
  (d > 0) andalso
  ((d <= 28 andalso  m = "February" ) orelse
   (d <= 30 andalso (m = "April" orelse
                     m = "June" orelse
                     m = "September" orelse
                     m = "November" )) orelse
   (d <= 31 andalso (m = "January" orelse
                     m = "March" orelse
                     m = "May" orelse
                     m = "July" orelse
                     m = "August" orelse
                     m = "October" orelse
                     m = "December" )));

(* 2.6: Doesn't check for validity of time. *)
type time = int * int * string;
fun is_before ((h1, m1, ampm1) : time, (h2, m2, ampm2) : time) =
  ampm1 = "AM" andalso
    (ampm2 = "PM" orelse
    (ampm1 = "AM" andalso (h1 < h2 orelse
                           h1 = h2 andalso m1 < m2)));

(* 2.7: *)
type old_eng_money = int * int * int;
fun to_pence ((pnd, sh, p) : old_eng_money) = p + 12 * (sh + 20*pnd);

fun to_oem n : old_eng_money =
  (n div 240, n mod 240 div 12, n mod 240 mod 12);

fun add_oem (amt1, amt2) = to_oem (to_pence amt1 + to_pence amt2);

fun sub_oem (amt1, amt2) = to_oem (to_pence amt1 - to_pence amt2);

(* 2.8: Correction: king -> int. *)

(* 2.9: Functions need fixed record types but selectors do not. *)

(* 2.10:
 * powoftwo 8 => 8 = 1 orelse (even 8 andalso ...)
 *            => even 8 andalso powoftwo (8 div 2)
 *            => powoftwo 4
 *            => 4 = 1 orelse (even 4 andalso ...)
 *            => even 4 andalaso powoftwo (4 div 2)
 *            => powoftwo 2
 *            => 2 = 1 orelse (even 2 andalso ...)
 *            => even 2 andalso powoftwo (2 div 2)
 *            => powoftwo 1
 *            => 1 = 1 orelse (even 1 andalso ...)
 *            => true
 *)

(* 2.11: Yes, as reduction steps show, storage will remain constant. *)

(* 2.12:
 * 2.0 * power (2.0 * 2.0, 29 div 2)
 * 2.0 * power (4.0 * 4.0, 14 div 2)
 * 2.0 * 16.0 * power (16.0 * 16.0, 7 div 2)
 * 2.0 * 16.0 * 256.0 * power (256.0 * 256.0, 3 div 2)
 * 2.0 * 16.0 * 256.0 * 65536.0
 * 2.0 * 16.0 * 16777216.0
 * 2.0 * 268435456.0
 * 536870912.0
 *)

(* 2.13: In the worst case, k = 2^m-1, O (lg n). *)

(* 2.14: An unnecessary extra computation step. *)

(* 2.15: Helps to think about it this way:
 *     fib 0 = 1
 *     fib 1 = 1
 *     fib n = +(fib (n-1), fib (n-2))
 * so for
 *     fib 4 = +(fib 3, fib 2)
 *           = +(+(fib 2, fib 1), +(fib 1, fib 0))
 *           = +(+(+(fib 1, fib 0), fib 1), +(1,1))
 *           = +(+(+(1, 1), 1), 2)
 *           = +(+(2, 1), 2)
 *           = +(3, 2)
 *           = 5
 * we see computation is still repeated here. Same with lazy,
 * UNLESS, memoization is used a la Haskell.
 *)

(* 2.16:
 * F(n) = F(n-1) + F(n-2)
 *      = (F(n-2) + F(n-3)) + F(n-2) = 2F(n-2) + F(n-3)
 * So for F(2n) = 2F(2(n-1)) + F(2(n-1)-1)
 * we see around a 2^n (really phi^n bound).
 *)

(* 2.17: F(k+n). *)

(* 2.18: needs stack apparently. *)

(* 2.19: Weird *)
fun even m = m mod 2 = 0;
fun half m = m div 2;

fun gcd (m, n) =
  if m = n then m else
  if m > n then gcd (n, m) else
    if even m then
      if even n then 2*gcd (half m, half n)
      else (* odd n *) gcd (half m, n)
    else
      gcd (half n - half m, m);

(* 2.20: We could use let instead of local. Makes little difference.
 * Nested can be harder to read. In findroot and sqroot, there were
 * multiple things, but local works for multiple as well. So, dunno.
 *)

(* 2.21: Online solution. Newton Raphson is **way** easier and simpler. *)
fun introot n =
  if n < 4 then (1, n-1)
  else
    let
      val (root, rem) = introot (n div 4)
      val i = 4*rem + n mod 4 (* rem. if root is even *)
      val j = i - 4*root - 1  (* rem. if root is odd *)
    in
      if j < 0 then (2*root, i) else (2*root+1, j)
    end;

(* PROOF: We use proof by induction.
 * Objective:
 * ----------
 * For a natural number n, find the greatest natural number a (and
 * thereby, the least natural number b) such that:
 *              a^2 <= n < (a+1)^2 and a^2 + b = n.
 *
 * As a consequence,
 *              a^2 + b < (a+1)^2
 *                    b < 2a + 1
 *                    b <= 2a.
 *
 * Base cases:
 * -----------
 *      0   =>  (1, ~1)     = 1*1 + ~1 = 0
 *      1   =>  (1, 0)      = 1*1 + 0  = 1
 *      2   =>  (1, 1)      = 1*1 + 1  = 2
 *      3   =>  (1, 2)      = 1*1 + 2  = 3
 *
 * Inductive hypothesis:
 * ---------------------
 * Assume introot n' = (rt, rem), i.e.
 * n' = rt^2 + rem where 0 <= rem <= 2*rt.
 *
 * Inductive step:
 * ---------------
 * Let n' = n div 4 and y = n (mod 4) > 0.
 * Let i = 4*rem + y > 0
 *     j = i - (4*rt + 1)
 *
 * We want the smallest remainder greater than 0.
 * For the even case, i is the remainder (see calculation below).
 * For the odd case, j is the remainder (see calculation below).
 *
 * If j < 0, then i < 4*rt + 1,
 *                4*rem <= 4*rt - y
 *                0 <= rem < rt, and so
 *
 *                n = 4n'+ y
 *                  = 4 (rt^2 + rem) + y
 *                  = (2*rt)^2 + 4*rem + y
 *                  = (2*rt)^2 + i
 *
 * Hence introot n = (2*rt, i) as required for the even case.
 *
 * If j > 0 then 4*rt + 1 - y < 4*rem i
 *               rt <= rem <= 2*rt
 *
 *               n = 4n' + y
 *                 = 4(rt^2 + rem) + y
 *                 = 4*rt^2 + 4*rt + 1 + 4*rem + y - (4*rt + 1)
 *                 = (2*rt+ 1)^2 + i - (4*rt + 1)
 *                 = (2*rt+ 1)^2 + j.
 *
 * Hence introot n = (2*rt+1, j) as required for the odd case.
 *)

(* 2.22: Equivalent to this: val pi = log2 and log2 = pi *)

(* 2.23: Try this, exponential. *)
fun p_n n    = if n <= 1 then 1 else 1 + sum_pn (n-1)
and sum_pn n = if n <= 1 then 1 else p_n n + sum_pn (n-1);

(* Efficient, because Maths ftw! *)
fun P_n n =
  let fun pn (count, acc) =
          if count <= 1 then acc else pn (count-1, 2*acc)
  in  pn (n, 1) end;

(* 2.24: *)
structure Real =
struct
  type t = real
  val zero = 0.0 : t
  fun sum  (a, b) = a + b : t
  fun diff (a, b) = a - b : t
  fun prod (a, b) = a * b : t
  fun quo  (a:t, b:t) = a / b : t   (* Said real*real -> t w/o it. *)
end;

(* 2.25: I could use higher-order functions and local..in..end to tidy
 * this up but for now, this suffices. *)
signature ARITH =
sig
  type t
  val zero : t
  val sum  : t * t -> t
  val diff : t * t -> t
  val prod : t * t -> t
  val quo  : t * t -> t
end;

structure Rational : ARITH =
struct
  type t = int * int
  val zero = (0, 1) : t
  fun norm ((a,b) : t) =
  let
    val (x,y) = (Int.abs a, Int.abs b)
    val hcf   = gcd (x,y)
    fun sgn x =  if x < 0 then ~1 else 1
    val sign  = sgn a * sgn b
    val (p,q) = if sign = 1 then (x, y) else (~x, y)
   in
     (p div hcf, q div hcf)
   end
  fun sum  ((a,b):t, (x,y):t) = norm (a*y + b*x, b*y)
  fun diff ((a,b):t, (x,y):t) = norm (a*y - b*x, b*y)
  fun prod ((a,b):t, (x,y):t) = norm (a*x, b*y)
  fun quo  (ab,      (x,y):t) = prod (ab, (y,x))
end;

(* 2.26: We see n=1 is used so n must be of type int. curr and prev must be of
 * the same type, either int or real. Because the funtion is annotated to return
 * int, we see curr has type int and so too does prev. Hence the type is:
 *              val itfib = fn : int * int * int => int. *)

(* 2.27: From k=0, we see k is of type int and the return is of type int.
 * However, unless f = fn : int -> int has been previously declared somewhere,
 * this presumably recursive call will cause a type mismatch in the clause. *)
