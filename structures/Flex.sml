(**** ML Programs from Chapter 4 of

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

structure Flex : FLEXARRAY = 
  struct
  datatype 'a array = Array of 'a tree * int;

  val empty = Array(Lf,0);

  fun length (Array(_,n)) = n;

  fun sub (Array(t,n), k) = 
      if 0<=k andalso k<n then Braun.sub(t,k+1) 
      else raise Subscript;

  fun update (Array(t,n), k, w) = 
      if 0<=k andalso k<n then Array(Braun.update(t,k+1,w), n)
      else raise Subscript;

  fun loext (Array(t,n), w) = Array(Braun.loext(t,w), n+1);

  fun lorem (Array(t,n)) = 
      if n>0 then Array(Braun.lorem t, n-1)
      else raise Size;

  fun hiext (Array(t,n), w) = Array(Braun.update(t,n+1,w), n+1);

  fun hirem (Array(t,n)) = 
      if n>0 then Array(Braun.delete(t,n) , n-1)
      else raise Size;

  end;
