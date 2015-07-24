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
structure Varray :> VARRAY =
  struct
  datatype 'a t = Modif of {limit: int, 
			   index: int ref,
			   elem: 'a ref, 
			   next: 'a t ref}
	       | Main of 'a Array.array;

  (*create a new array containing x in locations 0 to n-1. *)
  fun array (n,x) = 
	if n < 0  then  raise Size
	else  Modif{limit=n, index=ref 0, elem=ref x, 
		    next=ref(Main(Array.array(n,x)))};

  (* create from list *)
  fun fromList []      = raise Size
    | fromList (x::xs) =
        Modif {limit= length (x::xs), index=ref 0, elem=ref x,
          next = ref (Main(Array.fromList (x::xs)))};

  (*rerooting operation*)
  fun reroot (va as Modif{index, elem, next,...}) =
      case !next of
	 Main _ => va  (*have reached root*)
       | Modif _ => 
	   let val Modif{index=bindex,elem=belem,next=bnext,...} =
		     reroot (!next)
	       val Main ary = !bnext
	   in  bindex := !index;  
	       belem := Array.sub(ary, !index);
	       Array.update(ary, !index, !elem);
	       next := !bnext;
	       bnext := va;
	       va
	   end;

  (*subscripting*)
  fun sub (Modif{index,elem,next,...}, i) =
       case !next of
	  Main ary => Array.sub(ary,i)
	| Modif _ =>    if !index = i then !elem  
				      else sub(!next,i);

  (*plain update, no rerooting*)
  fun justUpdate(va as Modif{limit,...}, i, x) = 
	if  0<=i andalso i<limit  
	then Modif{limit=limit, index= ref i, 
		   elem=ref x, next=ref va}
	else raise Subscript;

  (*update and reroot*)
  fun update(va,i,x) = reroot(justUpdate(va,i,x));

  (* copy over *)
  fun copy (va as Modif {limit=l, index=i, elem=e, next=n}) =
    Modif {limit=l, index = ref (!i), elem = ref (!e),
        next = ref (Main (Array.tabulate (l, fn i => sub (va,i))))};

  end;
