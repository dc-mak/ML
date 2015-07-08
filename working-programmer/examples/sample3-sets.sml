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

(*** Finite sets ***)

(*membership in a list*)
infix mem;
fun x mem []  =  false
  | x mem (y::l)  =  (x=y) orelse (x mem l);

(*insertion into list if not already there*)
fun newmem(x,xs) = if x mem xs then  xs   else  x::xs;

(*insert the list of xs into the ys, adding no duplicates*)
fun union([],ys) = ys
  | union(x::xs, ys) = newmem(x, union(xs, ys));

fun inter([],ys) = []
  | inter(x::xs, ys) = 
        if x mem ys then x::inter(xs, ys)
                    else    inter(xs, ys);

fun powset ([], base) = [base]
  | powset (x::xs, base) = powset(xs, base) @ powset(xs, x::base);

fun cartprod ([], ys) = []
  | cartprod (x::xs, ys) =
        let val rest = cartprod(xs,ys)
            fun pairx [] = rest
              | pairx(y::ytail) = (x,y) :: (pairx ytail)
        in  pairx ys  end;
