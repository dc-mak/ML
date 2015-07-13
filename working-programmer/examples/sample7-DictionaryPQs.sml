(**** ML Programs from Chapter 7 of

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

(*** Dictionaries -- as a functor ***)

(** Linearly ordered types **)

signature ORDER = 
  sig
  type t
  val compare: t*t -> order
  end;

structure StringOrder: ORDER =
  struct
  type t = string;
  val compare = String.compare
  end;


functor Dictionary (Key: ORDER) : DICTIONARY = 
  struct

  type key = Key.t;

  abstype 'a t = Leaf
               | Bran of key * 'a * 'a t * 'a t
    with

    exception E of key;

    val empty = Leaf;

    fun lookup (Bran(a,x,t1,t2), b) =
          (case Key.compare(a,b) of
               GREATER => lookup(t1, b)
             | EQUAL   => x
             | LESS    => lookup(t2, b))
      | lookup (Leaf, b) = raise E b;

    fun insert (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | insert (Bran(a,x,t1,t2), b, y) =
          (case Key.compare(a,b) of
               GREATER => Bran(a, x, insert(t1,b,y), t2)
             | EQUAL   => raise E b
             | LESS    => Bran(a, x, t1, insert(t2,b,y)));

    fun update (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | update (Bran(a,x,t1,t2), b, y) =
          (case Key.compare(a,b) of
               GREATER => Bran(a, x, update(t1,b,y), t2)
             | EQUAL   => Bran(a, y, t1, t2)
             | LESS    => Bran(a, x, t1, update(t2,b,y)));
    end
  end;


(*This instance is required by sample9.sml and sample10.sml*)
structure StringDict = Dictionary (StringOrder);


(*Differs from the other PRIORITY_QUEUE by having a substructure*)
signature PRIORITY_QUEUE =
  sig
  structure Item : ORDER
  type t
  val empty    : t
  val null     : t -> bool
  val insert   : Item.t * t -> t
  val min      : t -> Item.t
  val delmin   : t -> t
  val fromList : Item.t list -> t
  val toList   : t -> Item.t list
  val sort     : Item.t list -> Item.t list
  end;
