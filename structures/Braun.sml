(**** Structure Braun from Chapter 4 of

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

structure Braun = 
  struct
  fun sub (Lf, _) = raise Subscript
    | sub (Br(v,t1,t2), k) =
	if k = 1 then v
	else if k mod 2 = 0 
	     then sub (t1, k div 2)
	     else sub (t2, k div 2);

  fun update (Lf, k, w) = 
	if k = 1 then Br (w, Lf, Lf)
	else raise Subscript
    | update (Br(v,t1,t2), k, w) =
	if k = 1 then Br (w, t1, t2)
	else if k mod 2 = 0 
	     then Br (v,  update(t1, k div 2, w),  t2)
	     else Br (v,  t1,  update(t2, k div 2, w));

  fun delete (Lf, n) = raise Subscript
    | delete (Br(v,t1,t2), n) =
	if n = 1 then Lf
	else if n mod 2 = 0 
	     then Br (v,  delete(t1, n div 2),  t2)
	     else Br (v,  t1,  delete(t2, n div 2));

  fun loext (Lf, w) = Br(w, Lf, Lf)
    | loext (Br(v,t1,t2), w) = Br(w, loext(t2,v), t1);

  fun lorem Lf = raise Size
    | lorem (Br(_,Lf,Lf)) = Lf 	(*No evens, therefore no odds either*)
    | lorem (Br(_, t1 as Br(v,_,_), t2)) = Br(v, t2, lorem t1);

  end;
