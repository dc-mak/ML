(**** ML Programs from Chapter 8 of

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

(*Note use of :> for abstraction*)
structure RingBuf :> RINGBUF =
  struct
  datatype 'a buf = Nil
		  | Node of 'a buf ref * 'a * 'a buf ref;
  datatype 'a t = Ptr of 'a buf ref;

  exception Empty;

  fun left (Node(lp,_,_)) = lp
    | left Nil = raise Empty;

  fun right (Node(_,_,rp)) = rp
    | right Nil = raise Empty;

  (*Must be a function, as each ring buffer needs a separate reference.
     Also the type checker would reject the polymorphic reference.*)
  fun empty() = Ptr(ref Nil);

  fun null (Ptr p) = case !p of Nil => true
			      | Node(_,x,_) => false;

  fun label (Ptr p) = case !p of Nil => raise Empty
			       | Node(_,x,_) => x;

  fun moveLeft (Ptr p) = (p := !(left(!p)));

  fun moveRight (Ptr p) = (p := !(right(!p)));

  (*Insert to left of the window, which is unchanged unless empty. *)
  fun insert (Ptr p, x) =
      case !p of
	  Nil => 
	      let val lp = ref Nil
		  and rp = ref Nil
		  val new = Node(lp,x,rp)
	      in  lp := new;  rp := new;  p := new  end
	| Node(lp,_,_) =>
	      let val new = Node(ref(!lp), x, ref(!p))
	      in  right(!lp) := new;  lp := new  end;

  (*Delete the current node, raising Empty if there is none.
    Observe the comparison of left refs to see whether they are identical. *)
  fun delete (Ptr p) =
      case !p of
	  Nil => raise Empty
	| Node(lp,x,rp) =>
	     (if left(!lp) = lp then
             p := Nil
	      else
            (right (!lp) := !rp;
             left (!rp)  := !lp;
             p           := !rp);
          x)

  end;
