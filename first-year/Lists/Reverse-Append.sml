(* Prelim CS2 Question 2
*  --------------------
*  Analyse the time complexity of: *)
fun rvs [] = ([])
  | rvs (a::b) = app (rvs b) [a]

and app [] b = b
  | app a b = app (rvs (tl (rvs a))) (hd(rvs a)::b);

(*
   R(0)         =   1
   R(n)         =   A(n-1, 1) + R(n-1)
   R(n-1)       =   A(n-2, 1) + R(n-2)
   
   A(0, t)      =   1
   A(s, t)      =   A(s-1, t+1) + R(n-1) + 2R(n)
   A(n-1, 1)    =   A(n-2, 2) + R(n-2) + 2R(n-1)
   
   R(n)         =   A(n-2, 2) + R(n-2) + 3R(n-1)
                =   R(n-1) + 3R(n-1)
                =   4R(n-1)
*)

val p = ref 0;

fun dbg_rvs []     = (p := !p + 1; print "1"; [])
  | dbg_rvs (a::b) = (p := !p + 1; print "2"; dbg_app (dbg_rvs b) [a])

and dbg_app [] b   = (p := !p + 1; print "3"; b)
  | dbg_app a b    = (p := !p + 1; print "4";
                      dbg_app (dbg_rvs (tl (dbg_rvs a)))
                                       (hd (dbg_rvs a)::b));

val result = ref "Result:";
dbg_rvs [1];
result := !result^" R(1)="^Int.toString(!p);
p := 0;
dbg_rvs [1, 2];
result := !result^" R(2)="^Int.toString(!p);
p := 0;
dbg_rvs [1,2,3];
result := !result^" R(3)="^Int.toString(!p);
p := 0;
dbg_rvs [1,2,3,4];
result := !result^" R(4)="^Int.toString(!p);
p := 0;
dbg_rvs [1,2,3,4,5,6,7,8,9];
result := !result^" R(9)="^Int.toString(!p);
p := 0;
!result;
