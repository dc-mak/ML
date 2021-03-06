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

signature FLEXARRAY = 
  sig
  type 'a array
  val empty:  'a array
  val length: 'a array -> int
  val sub:    'a array * int -> 'a
  val update: 'a array * int * 'a -> 'a array
  val loext:  'a array * 'a -> 'a array
  val lorem:  'a array -> 'a array
  val hiext:  'a array * 'a -> 'a array
  val hirem:  'a array -> 'a array
  end;
