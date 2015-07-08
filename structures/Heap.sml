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

structure Heap : PRIORITY_QUEUE = 
  struct
  type item = real;
  type t = item tree;

  val empty = Lf;

  fun null Lf = true
    | null (Br _) = false;

  fun min (Br(v,_,_)) = v;

  fun insert(w: real, Lf) = Br(w, Lf, Lf)
    | insert(w, Br(v, t1, t2)) =
	if w <= v then Br(w, insert(v, t2), t1)
		  else Br(v, insert(w, t2), t1);

  fun leftrem (Br(v,Lf,Lf)) = (v, Lf)
    | leftrem (Br(v,t1,t2)) = 
        let val (w, t) = leftrem t1
	in  (w, Br(v,t2,t))  end;

  fun siftdown (w:real, Lf, Lf) = Br(w,Lf,Lf)
    | siftdown (w, t as Br(v,Lf,Lf), Lf) =
        if w <= v then Br(w, t, Lf)
                  else Br(v, Br(w,Lf,Lf), Lf)
    | siftdown (w, t1 as Br(v1,p1,q1), t2 as Br(v2,p2,q2)) =
        if w <= v1 andalso w <= v2 then Br(w,t1,t2)
        else if v1 <= v2 then Br(v1, siftdown(w,p1,q1), t2)
           (* v2 < v1 *) else Br(v2, t1, siftdown(w,p2,q2));

  fun delmin Lf = raise Size
    | delmin (Br(v,Lf,_)) = Lf
    | delmin (Br(v,t1,t2)) = 
        let val (w,t) = leftrem t1
	in  siftdown (w,t2,t)  end;

  fun heapify (0, vs) = (Lf, vs)
    | heapify (n, v::vs) =
	let val (t1, vs1) = heapify (n div 2, vs)
	    val (t2, vs2) = heapify ((n-1) div 2, vs1)
	in  (siftdown (v,t1,t2), vs2)  end;

  fun fromList vs = #1 (heapify (length vs, vs));

  fun toList (t as Br(v,_,_)) = v :: toList(delmin t)
    | toList Lf = [];

  fun sort vs = toList (fromList vs);
  end;
