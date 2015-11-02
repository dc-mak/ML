(**** Signature SEQUENCE from Chapter 5 of

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

Changes made by Dhruv Makwana:
    - Changed "from" to "tabulate", since it's more general.
    - Added concat (formerly enumerate) to the signature.
****)


(*Type seq is free in this signature!*)
signature SEQUENCE = 
  sig
  exception Empty
  val cons :        'a * 'a seq -> 'a seq
  val null :        'a seq -> bool
  val hd :          'a seq -> 'a
  val tl :          'a seq -> 'a seq
  val fromList  :   'a list -> 'a seq
  val toList  :     'a seq -> 'a list
  val take :        'a seq * int -> 'a list
  val drop :        'a seq * int -> 'a seq
  val @  :          'a seq * 'a seq -> 'a seq
  val interleave :  'a seq * 'a seq -> 'a seq
  val map :         ('a -> 'b) -> 'a seq -> 'b seq
  val filter :      ('a -> bool) -> 'a seq -> 'a seq
  val iterates :    ('a -> 'a) -> 'a -> 'a seq
  val tabulate :    (int -> 'a) -> 'a seq
  val concat :      'a seq seq -> 'a seq
  end;
