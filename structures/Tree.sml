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

structure Tree =
  struct
  fun size Lf = 0
    | size (Br(v,t1,t2)) = 1 + size t1 + size t2;

  fun depth Lf = 0
    | depth (Br(v,t1,t2)) = 1 + Int.max (depth t1, depth t2);

  fun reflect Lf = Lf
    | reflect (Br(v,t1,t2)) = 
      Br(v, reflect t2, reflect t1);

  fun preord (Lf, vs) = vs
    | preord (Br(v,t1,t2), vs) =
      v :: preord (t1, preord (t2, vs));

  fun inord (Lf, vs) = vs
    | inord (Br(v,t1,t2), vs) =
      inord (t1, v::inord (t2, vs));

  fun postord (Lf, vs) = vs
    | postord (Br(v,t1,t2), vs) =
      postord (t1, postord (t2, v::vs));

  fun balpre  []    = Lf
    | balpre(x::xs) =
    let val k = length xs div 2
    in  Br(x, balpre(List.take(xs,k)),  balpre(List.drop(xs,k)))
    end;

  fun balin [] = Lf
    | balin xs =
    let val k = length xs div 2
        val y::ys = List.drop(xs,k)
    in  Br(y, balin (List.take(xs,k)), balin ys)
    end;

  fun balpost xs = reflect (balpre (rev xs));
  end;
