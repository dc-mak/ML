(**** ML Programs from Chapter 5 of

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


(*** Functions as values ***)

fun insort lessequal = 
    let fun ins (x, []) = [x]
          | ins (x, y::ys) = 
              if lessequal(x,y) then x::y::ys 
              else y :: ins (x,ys)
        fun sort [] = []
          | sort (x::xs) = ins (x, sort xs)
    in  sort  end;

(*Sections*)
fun secl x f y = f(x,y);
fun secr f y x = f(x,y);

fun summation f m =
    let fun sum (i,z) : real =
            if  i=m  then  z  else  sum (i+1, z + (f i))
    in  sum(0, 0.0)  end;

fun takewhile pred [] = []
  | takewhile pred (x::xs) = 
        if  pred x  then  x :: takewhile pred xs  
        else  [];

fun dropwhile pred [] = []
  | dropwhile pred (x::xs) = 
        if  pred x  then  dropwhile pred xs  
        else  x::xs;

infix mem;
fun x mem xs = List.exists (secr op= x) xs;

fun pair x y = (x,y);

fun cartprod (xs, ys) = 
    foldr (fn (x, pairs) => 
                 foldr (fn (y,l) => (x,y)::l) pairs ys)
              [] xs;

fun repeat f n x = 
    if n>0  then  repeat f (n-1) (f x)
            else  x;
