(**** ML Programs from Chapter 3 of

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

(*** Matrix transpose ***)

fun headcol []    = []
  | headcol ((x::_) :: rows) = x :: headcol rows;

fun tailcols []    = []
  | tailcols ((_::xs) :: rows) = xs :: tailcols rows;

(*LOOPS given empty list!*)
fun transp ([]::rows) = []
  | transp rows = headcol rows :: transp (tailcols rows);


(*** Matrix product ***)

fun dotprod([], []) = 0.0
  | dotprod(x::xs,y::ys) = x*y + dotprod(xs,ys);

fun rowprod(row, []) = []
  | rowprod(row, col::cols) =
        dotprod(row,col) :: rowprod(row,cols);

fun rowlistprod([], cols) = []
  | rowlistprod(row::rows, cols) =
        rowprod(row,cols) :: rowlistprod(rows,cols);

fun matprod(rowsA,rowsB) = rowlistprod(rowsA, transp rowsB);


(*** Gaussian Elimination (from Sedgewick, Chapter 5) ***)

(*the first row with absolutely greatest head*)
fun pivotrow [row] = row : real list
  | pivotrow (row1::row2::rows) =
      if abs(hd row1) >= abs(hd row2)
      then pivotrow(row1::rows)
      else pivotrow(row2::rows);

(*the matrix excluding the first row with head p*)
fun delrow (p, []) = []
  | delrow (p, row::rows) =
      if Real.==(p, hd row) then rows
      else row :: delrow(p, rows);

fun scalarprod(k, []) = [] : real list
  | scalarprod(k, x::xs) = k*x :: scalarprod(k,xs);

fun vectorsum ([], []) = [] : real list
  | vectorsum (x::xs,y::ys) = x+y :: vectorsum(xs,ys);

fun gausselim [row] = [row]
  | gausselim rows =
      let val p::prow = pivotrow rows
          fun elimcol [] = []
            | elimcol ((x::xs)::rows) =
                  vectorsum(xs, scalarprod(~x/p, prow))
                  :: elimcol rows
      in  (p::prow) :: gausselim(elimcol(delrow(p,rows)))
      end;

fun solutions [] = [~1.0]
  | solutions((x::xs)::rows) =
      let val solns = solutions rows
      in ~(dotprod(solns,xs)/x) :: solns  end;
