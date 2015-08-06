structure Rat :> NUMBER where type t' = int * int =
  struct
  (* Auxilliary *)
  fun gcd (0,n) = n
    | gcd (m,n) = gcd (n mod m, m)

  fun norm (a,b) =
    let val hcf  = gcd (Int.abs a, Int.abs b)
        and sgn  = Int.sign b
    in  (sgn * a div hcf, sgn * b div hcf) end

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
    let val ((a,b),(x,y)) = (norm ab, norm xy)
    in  Int.compare (a*x, b*y) end

  fun eq (a,b) = compare (a,b) = EQUAL
  end;
