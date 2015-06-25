(* Create a seq seq of all the elements crossed *)
fun cross Nil yf = Nil
  | cross xf Nil = Nil
  | cross (Cons(x,xf)) yf =
  Cons(seqMap (fn y => (x,y)) yf, fn () => cross (xf()) yf);

(* Gather up the values AND advance the list *)
fun gather Nil depth = (Nil, Nil)
  | gather yff 0 = (Nil, yff)
  | gather (Cons(Nil,yff)) depth = (print "Passed\n"; gather (yff()) depth)
  | gather (Cons(Cons(y,yf),yff)) depth = 
  let
    val (zf, zff) = gather (yff()) (depth-1)
  in
    (Cons(y, fn () => zf), Cons(yf(), fn () => zff))
  end;

(* Since we know each list WILL terminate *)
fun append Nil yf = yf
  | append (Cons(x,xf)) yf = Cons(x, fn() => append (xf()) yf);

(* Let Cantor be proud *)
fun seqCant a b =
let
  fun diag Nil depth = Nil
    | diag yff depth = 
    case (gather yff depth) of 
         (Nil, rff) => diag rff depth
       | (Cons(r,rf), rff) =>
          Cons(r, fn () => append (rf()) (diag rff (depth+1)))
in
  diag (cross a b) 1
end;

fun nat6 n = if n < 6 then Cons(n, fn () => nat6 (n+1)) else Nil;
fun nat n = Cons(n, fn () => nat (n+1));

(*
use "C2.sml";
*)
(*
val b = nat6 0; val a = nat 0; val ab = seqCant a b;
seqTl ab;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
seqTl it;
print "Note that the depth is increasing despite the finite second list.\n";
*)
