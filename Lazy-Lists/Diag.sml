use "Useful.sml";

fun seqDiag xff =
let 
  fun diag n Nil = Nil
    | diag n (Cons(xf, xff)) =
    Cons(seqNth n xf, fn () => diag (n+1) (xff()))
in
  diag 0 xff
end;

fun seqApp f Nil yf = Nil
  | seqApp f xf Nil = xf
  | seqApp f (Cons(x,xf)) yf  =
  Cons(seqMap (f x) yf, fn () => seqApp f (xf()) yf);

(* See seqConcat *)
