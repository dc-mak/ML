structure Rat :> NUMBER where type t' = int * int =
  struct
  (* Auxilliary *)
  fun gcd (0,n) = n
    | gcd (m,n) = gcd (n mod m, m)

  fun norm (a,b) =
    let
      val (x,y) = (Int.abs a, Int.abs b)
      val hcf   = gcd (x,y)
      fun sgn x =  if x < 0 then ~1 else 1
      val sign  = sgn a * sgn b
      val (p,q) = if sign = 1 then (x, y) else (~x, y)
    in
      (p div hcf, q div hcf)
    end

  (* Signature related *)
  type t' = int * int
  datatype t = Frac of t'

  val zero = Frac (0, 1) 
  val one  = Frac(1,1)

  exception Undefined
  fun make (a,b) = if b = 0 then raise Undefined else Frac(norm(a,b))
  fun dest (Frac(a,b)) = norm (a,b)
  fun toString (Frac(a,b)) = "("^Int.toString a^"/"^Int.toString b^")"

  fun sum  (Frac(a,b), Frac(x,y)) = Frac(norm (a*y + b*x, b*y))
  fun prod (Frac(a,b), Frac(x,y)) = Frac(norm (a*x, b*y))
  
  fun inv (Frac(a,b)) = Frac(norm(b, a))
  fun neg (Frac(a,b)) = Frac(norm(~a,b))

  fun abs (Frac(a,b)) = Frac(norm(Int.abs a, Int.abs b))

  fun diff (ab, xy) = sum  (ab, neg xy)
  fun divd (ab, xy) = prod (ab, inv xy)

  fun compare (Frac ab, Frac xy) =
      case (norm ab, norm xy) of ((a,b),(x,y)) => Int.compare (a*x, b*y)

  fun eq (a,b) = compare (a,b) = EQUAL
  end;
