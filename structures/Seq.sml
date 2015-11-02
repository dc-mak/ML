(**** Signature SEQUENCE from Chapter 5 of

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

Changes made by Dhruv Makwana:
    - Changed "from" to "tabulate", since it's more general.
    - Added concat (formerly enumerate) to the signature.
****)

structure Seq : SEQUENCE =
  struct
  exception Empty;

  fun hd (Cons(x,xf)) = x
    | hd Nil = raise Empty;

  fun tl (Cons(x,xf)) = xf()
    | tl Nil = raise Empty;

  fun cons(x,xq) = Cons(x, fn()=>xq);

  fun null (Cons _) = false
    | null Nil      = true;

  fun fromList [] = Nil
    | fromList (x::xs) = Cons (x, fn()=> fromList xs)

  fun toList Nil = []
    | toList (Cons(x,xf)) = x :: toList (xf());

  fun take (xq, 0) = []
    | take (Nil, n) = raise Subscript
    | take (Cons(x,xf), n) = x :: take (xf(), n-1);

  fun drop (xq, 0) = xq
    | drop (Nil, n) = raise Subscript
    | drop (Cons(x,xf), n) = drop (xf(), n-1);

  infix 5 @;
  fun Nil @ yq = yq
    | (Cons(x,xf)) @ yq = Cons(x, fn()=> (xf()) @ yq);

  fun interleave (Nil,    yq) = yq
    | interleave (Cons(x,xf), yq) = 
	  Cons(x, fn()=> interleave(yq, xf()));

  (** functionals for sequences **)
  fun map f Nil  = Nil
    | map f (Cons(x,xf)) = Cons(f x, fn()=> map f (xf()));

  fun filter pred Nil = Nil
    | filter pred (Cons(x,xf)) =
	  if pred x then Cons(x, fn()=> filter pred (xf()))
		    else filter pred (xf());

  fun iterates f x = Cons(x, fn()=> iterates f (f x));

  fun tabulate f =
      let fun tab k = Cons(f k, fn () => tab (k+1)) in tab 0 end

  (* WARNING: Will not terminate for infinite sequence of Nils *)
  fun concat Nil                     = Nil
    | concat (Cons(Nil, xqf))        = concat (xqf())
    | concat (Cons(Cons(x,xf), xqf)) =
          Cons(x, fn()=> interleave(concat (xqf()), xf()));
  end;
