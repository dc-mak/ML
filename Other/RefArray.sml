(*1994, Paper 1, Question 6*)
(* Dhruv Makwana *)

(* Array: value, right ref and up ref *)
datatype A = Z | Cell of int ref * A ref * A ref;

fun mkrow 0 = Cell (ref 0, ref Z, ref Z)
  | mkrow n = Cell (ref 0, ref (mkrow (n-1)), ref Z);

(* Attach the first row underneath the second *)
fun zip (Z,Z) = Z
  | zip (Cell(i,r,u), Z) = Cell (i,r,ref Z)
  | zip (Z, c) = c
  | zip (Cell(i1,r1,u1), c2 as Cell(i2,r2,u2)) =
    Cell (i1, ref (zip (!r1,!r2)), ref c2);

(* Make an array by zipping a row underneath
* an array of one less size. *)
fun mkarr (m,0) = mkrow m
  | mkarr (m,n) = zip (mkrow m, mkarr(m,n-1));

datatype dir = Right | Up;

(* increment path cells = ipc *)
fun ipc Z _ = ()
  | ipc _  [] = ()
  | ipc (Cell(i,r,u)) (d::ds) =
  ((case d of 
        Right   => ipc (!r) ds
      | Up      => ipc (!u) ds); 
      i := !i +1);
