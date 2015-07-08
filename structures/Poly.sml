(**** ML Programs from Chapter 3 of

  ML for the Working Programmer, 2nd edition
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1996)

Copyright (C) 1996 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)

use "structures/ARITH.sig";

(*An obscure built-in infix in some compilers; we need it nonfix*)
nonfix rem;

signature POLY =
  sig
  include ARITH
  val show : t -> string
  end;

structure Poly : POLY =
  struct
  type t = (int*real)list;

  val zero = [];

  (** Sum of two polynomials **)
  fun sum ([], us)               = us : t
    | sum (ts, [])               = ts
    | sum ((m,a)::ts, (n,b)::us) =
	     if m>n then (m,a) :: sum (ts, (n,b)::us)
	else if n>m then (n,b) :: sum (us, (m,a)::ts)
	else (*m=n*) 
	     if Real.==(a+b, 0.0) then sum (ts, us)
			          else (m, a+b) :: sum (ts, us);

  (** Product of a term and a polynomial **)
  fun termprod ((m,a), [])        = [] : t
    | termprod ((m,a), (n,b)::ts) = 
	(m+n, a*b) :: termprod ((m,a), ts);

  (** Difference of two polynomials **)
  fun diff ([], us)               = termprod ((0, ~1.0), us)
    | diff (ts, [])               = ts
    | diff ((m,a)::ts, (n,b)::us) =
	     if m>n then (m,a) :: diff (ts, (n,b)::us)
	else if n>m then (n,b) :: diff (us, (m,a)::ts)
	else (*m=n*) 
	     if Real.==(a-b, 0.0) then diff (ts, us)
			          else (m, a-b) :: diff (ts, us);

  (** Product of two polynomials **)

  (*Product by balanced merging; could improve speed, like with merge sort*)
  fun prod ([], us)      = []
    | prod ([(m,a)], us) = termprod ((m,a), us)
    | prod (ts, us)      =
	let val k = length ts div 2
	in  sum (prod (List.take(ts,k), us), prod (List.drop(ts,k), us))
	end;

  (** Division: quotient and remainder **)

  (*Division by zero -- empty polynomial -- raises exception Match*)
  fun quorem (ts, (n,b)::us) =
    let fun dividing ([],        qs) = (rev qs, [])
	  | dividing ((m,a)::ts, qs) =
	      if m<n then (rev qs, (m,a)::ts)
	      else dividing (sum (ts, termprod ((m-n, ~a/b), us)),
			(m-n, a/b) :: qs)
    in  dividing (ts, [])  end;

  fun quo (ts,us) = #1(quorem (ts,us))
  and rem (ts,us) = #2(quorem (ts,us));

  (*A bad gcd algorithm: results are not in primitive form*)
  fun gcd ([], us) = us
    | gcd (ts, us) = gcd (rem (us,ts), ts);

  fun show ts =
    let
      fun term (m,a) =
      let val pow = Int.toString m
          val coeff = Real.toString a
      in if m = 0 then coeff else
         if m = 1 then coeff^"x"
                    else coeff^"x"^pow
      end
      
      fun s ([]         , str) = str
        | s ([t]  , str) = term t
        | s (t::ts, str) = s (ts, str^term t^" ")
    in s (ts, "") end;
  end;
