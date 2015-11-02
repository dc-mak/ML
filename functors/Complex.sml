signature COMP = 
  sig
  include ARITH
  structure Num : NUMBER
  val re : t -> Num.t
  val im : t -> Num.t
  end;

functor Complex (Num : NUMBER) :> COMP where type t' = Num.t * Num.t = 
  struct

  (* Auxilliary *)
  structure Num = Num
  val ~   = Num.neg
  val op+ = Num.sum
  val op- = Num.diff
  val op* = Num.prod
  val op/ = Num.divd

  (* Signature related *)
  type t'    = Num.t * Num.t
  datatype t = Comp of t'
  val zero = Comp (Num.zero, Num.zero)

  fun make (a, b)       = Comp (a, b)
  fun dest (Comp (a,b)) = (a,b)

  fun re (Comp(a,b)) = a
  fun im (Comp(a,b)) = b

  fun toString (Comp(a,b)) =
    let val b_str = case Num.compare (b, Num.zero) of
                         LESS => " - "^Num.toString (~b)
                       | _    => " + "^Num.toString b
    in  Num.toString a^b_str end

  fun neg  (Comp(a,b))            = Comp(~a, ~b)
  fun sum  (Comp(a,b), Comp(x,y)) = Comp(a + x, b + y)
  fun diff (Comp(a,b), Comp(x,y)) = Comp(a - x, b - y)
  fun prod (Comp(a,b), Comp(x,y)) = Comp(a*x - b*y, a*y + b*x)

  fun inv (Comp(x,y)) = let val k = x*x + y*y in Comp(x/k, ~y/k) end
  fun divd (a, x)     = prod (a, inv x)

  fun eq (Comp(a,b), Comp(x,y)) = Num.eq (a,x) andalso Num.eq (b,y)
  end;
