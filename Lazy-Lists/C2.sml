(* Works for given test case only *)
fun gather Nil depth = ((Nil, Nil), depth)
  | gather yff 0 = ((Nil, yff), 0)
  | gather (Cons(Nil,yff)) depth = (print "Break\n"; ((Nil, yff()), depth))
  | gather (Cons(Cons(y,yf),yff)) depth = 
  let
    val ((zf, zff), d) = gather (yff()) (depth-1)
  in
    ((Cons(y, fn () => zf), Cons(yf(), fn () => zff)), d)
  end;

(* Let Cantor be proud *)
fun seqCant a b =
let
  fun diag Nil depth = Nil
    | diag yff depth = 
    let
      val (a, d) = gather yff depth
      val d = if d = 0 then (depth+1) else (d-1)
    in
      case a of
           (Nil, rff) => diag rff d
         | (Cons(r,rf), rff) =>
             Cons(r, fn () => append (rf()) (diag rff d))
    end
in
  diag (cross a b) 1
end;

fun nat6 n = if n < 6 then Cons(n, fn () => nat6 (n+1)) else Nil;
fun nat n = Cons(n, fn () => nat (n+1));

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
