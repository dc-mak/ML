structure Rational : ARITH =
  struct
  type t = int * int

  val zero = (0, 1) 

  fun norm (a,b) =
    let val hcf  = gcd (Int.abs a, Int.abs b)
        and sgn  = Int.sign b
    in  (sgn * a div hcf, sgn * b div hcf) end

  fun sum  ((a,b), (x,y)) = norm (a*y + b*x, b*y)
  fun diff ((a,b), (x,y)) = norm (a*y - b*x, b*y)
  fun prod ((a,b), (x,y)) = norm (a*x, b*y)
  fun quo  (ab,      (x,y)) = prod (ab, (y,x))
  end;
