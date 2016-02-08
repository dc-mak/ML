(* Dhruv Makwana, Trinity College, dcm41 *)
(* Q1 *)
fun sumt n =
let
  fun sum 0 total = total
    | sum n total = sum (n-1) (total/2.0 + 1.0)
in
  sum n 0.0
end;

(* Q2 *)
fun eapprox n = 
let
  fun eapp 1 total = total
    | eapp n total = eapp (n-1) (1.0 + total/real n)
in
  eapp n 0.0
end;

(* Q3 *)
fun exp (z,n) =
let
  fun e 1 total = total
    | e n total = e (n-1) (1.0 + z*total/real (n-1))
in
  e n 1.0
end;

exp (1.0, 3);
exp (1.0, 78);
exp (2.0, 3);

(* 
 * val sumt = fn: int -> real
 * val eapprox = fn: int -> real
 * val exp = fn: real * int -> real
 * val it = 2.5: real
 * val it = 2.718281828: real
 * val it = 5.0: real 
 *)
