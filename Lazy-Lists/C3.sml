fun mapq f xf 0 = xf
  | mapq f Nil n = Nil
  | mapq f (Cons(x,xf)) n =
    Cons(f x, fn () => mapq f (xf()) (n-1));

fun chain xf 0 = Nil
  | chain Nil n = Nil
  | chain (Cons(x,xf)) n =
  Cons(x, fn () => chain (xf()) (n-1));

fun cantor a b =
let
  fun cntr Nil depth = Nil
    | cntr yff depth =
    let
      val next = mapq seqTl yff (depth)
      val lst  = chain next (depth)
    in
      Cons
in
  cntr (cross a b) 1
end;
