structure Rational : ARITH =
  struct
  type t = int * int

  val zero = (0, 1) 

  fun norm ((a,b) ) =
    let
      val (x,y) = (Int.abs a, Int.abs b)
      val hcf   = gcd (x,y)
      fun sgn x =  if x < 0 then ~1 else 1
      val sign  = sgn a * sgn b
      val (p,q) = if sign = 1 then (x, y) else (~x, y)
    in
       (p div hcf, q div hcf)
    end

  fun sum  ((a,b), (x,y)) = norm (a*y + b*x, b*y)
  fun diff ((a,b), (x,y)) = norm (a*y - b*x, b*y)
  fun prod ((a,b), (x,y)) = norm (a*x, b*y)
  fun quo  (ab,      (x,y)) = prod (ab, (y,x))
  end;
