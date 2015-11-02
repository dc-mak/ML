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

(**** Lazy sequences using references ****)

(*note weak type variables in cons, @, map, filter, cycle... 
  everything that builds a sequence*)
signature IMP_SEQUENCE = 
  sig
  type 'a t
  exception Empty
  val empty:        'a t
  val cons:         'a * (unit -> 'a t) -> 'a t
  val null:         'a t -> bool
  val hd:           'a t -> 'a
  val tl:           'a t -> 'a t
  val fromList:     'a list -> 'a t
  val toList:       'a t -> 'a list
  val take:         'a t * int -> 'a list
  val drop :        'a t * int -> 'a t
  val @ :           'a t * 'a t -> 'a t
  val interleave :  'a t * 'a t -> 'a t
  val map:          ('a -> 'b) -> 'a t -> 'b t
  val filter:       ('a -> bool) -> 'a t -> 'a t
  val concat:       'a t t -> 'a t
  val tabulate :    (int -> 'a) -> 'a t
  val cycle:        ((unit -> 'a t) -> 'a t) -> 'a t
  end;

