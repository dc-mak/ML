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

(*note use of :> for abstraction*)
structure ImpSeq :> IMP_SEQUENCE =
  struct
  datatype 'a t  = Nil
		 | Cons of 'a * ('a t) ref
		 | Delayed of unit -> 'a t;

  exception Empty;

  fun delay xf = ref(Delayed xf);

  (*sequence "constructors" for export*)
  val empty = Nil;

  fun cons(x,xf) = Cons(x, delay xf);

  (*gets tail value, perhaps with the side effect of storing it*)
  fun force xp =
    case !xp of Delayed f => let val s = f() in  xp := s; s end
              | s         => s;

  (** these functions do not expect delayed -- it is only permissible
      in the tail of a cons, where it is enclosed in a reference **)

  fun null Nil = true
    | null (Cons _) = false;

  fun hd Nil = raise Empty
    | hd (Cons(x,_)) = x;

  fun tl Nil = raise Empty
    | tl (Cons(_,xp)) = force xp;

  fun fromList [] = Nil
    | fromList (x::xs) = cons(x, fn()=> fromList xs);

  fun toList Nil = []
    | toList (Cons(x,xp)) = x :: toList (force xp);

  fun take (xq, 0) = []
    | take (Nil, n) = []
    | take (Cons(x,xp), n) = x :: take (force xp, n-1);

  fun drop (xq, 0) = xq
    | drop (Nil, n) = Nil
    | drop (Cons(x,xp), n) = drop (force xp, n-1);

  fun Nil @ yq = yq
    | (Cons(x,xp)) @ yq =
        Cons(x, delay(fn()=> (force xp) @ yq));

  fun interleave (Nil,    yq) = yq
    | interleave (Cons(x,xp), yq) = 
	  Cons(x, delay (fn()=> interleave(yq, force xp)));

  (** functionals for sequences **)
  fun map f Nil  = Nil
    | map f (Cons(x,xp)) = 
          Cons(f x, delay(fn()=> map f (force xp)));

  fun filter pred Nil = Nil
    | filter pred (Cons(x,xp)) =
	  if pred x 
          then Cons(x, delay(fn()=> filter pred (force xp)))
	  else filter pred (force xp);

  (*concatenate a sequence of sequences, taking care not to loop! *)
   fun concat xqq =
     if null xqq then empty else
     if null(hd xqq) then concat(tl xqq) else
       cons(hd(hd xqq),  
		    fn()=> tl(hd xqq) @ concat(tl xqq));

   fun tabulate f =
     let fun tab k = Cons(f k, delay(fn () => tab (k+1))) in tab 0 end;

  (*idea thanks to C. Reade, see appendix 3 of his book;
    seqfn must not call its argument outside of a closure;
    lest it get nil rather than the cycle. *)
  fun cycle seqfn =
      let val knot = ref Nil
      in  knot := seqfn (fn()=> !knot);  !knot  end;
  end;
