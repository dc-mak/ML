(**** Structure Matrix from Chapter 3 (and online solutions) of

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

(* TODO: Redo with functionals. *)
structure Matrix : ARITH =
  sig
  
  end;

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
(* 3.21: *)
fun transp2 []      = []
  | transp2 (y::ys) =
  let
    fun singleton []      = []
      | singleton (x::xs) = [x]::singleton xs;

    fun join ([],    ys)    = []
      | join (xs,    [])    = singleton xs
      | join (x::xs, y::ys) = (x::y)::join(xs,ys);
  in
    join(y, transp2 ys)
  end;

(* 3.22: *)
fun neglist []      = [] : real list
  | neglist (y::ys) = ~y::neglist ys;

fun matneg []      = []
  | matneg (x::xs) = neglist x::matneg xs;

(* 3.23: inexhaustive pattern matching so that we know when matrices are
 * not of the same dimensions. *)
fun matadd ([], [])       = []
  | matadd (x::xs, y::ys) =
  let
    fun rowadd ([], [])       = [] : real list
      | rowadd (t::ts, u::us) = t+u::rowadd(ts,us)
  in
    rowadd(x,y)::matadd(xs,ys)
  end;

(* 3.24: Assume the columns are linearly independent. That means no column
 * or row can be expressed as a linear combination of any other row. The only
 * place division occurs in gausselim is by p. Since pivotrows selects the
 * largest absolute value, no zeros wil occur... unless, we have column of all
 * zeros - not possible with linear independence. *)

(* 3.25: pivotrows finds the first row with the largest head value and
 * delrow finds and deletes the first. *)

(* This could have all been replaced with a bunch of maps *)
fun cons (x, [])    = []
  | cons (x, y::ys) = (x::y)::cons(x, ys)

fun rmcols []      = []
  | rmcols (x::xs) = xs::cons (x, rmcols xs)

(* 3.26: *)
fun matdet [[x]]          = x
  | matdet [[a,b], [c,d]] = a*d - b*c
  | matdet (z::zs)        =
  let
    fun submats []      = []
      | submats (y::ys) = rmcols y::submats ys

    fun subdets []      = []
      | subdets (m::ms) = matdet m::subdets ms
  in
    dotprod (z, subdets (transp2 (submats zs)))
  end;

(* Online solution: modified gausselim *)
(* This should never be passed [] hence inexhaustive patterns. *)
fun signed_gausselim [row]  = ([row], 1.0)
  | signed_gausselim (rows) =
  let
    (* pivotrow will never return [], hence inexhaustive patterns. *)
    val p::prow = pivotrow rows
    (* Check only first value in matrix. *)
    val samerow = Real.== (abs (hd (hd rows)), abs p)
    fun elimcol []              = []
      | elimcol ((x::xs)::rows) =
      vectorsum (xs, scalarprod(~x/p, prow))::elimcol rows
    val (g_rows, odd) = signed_gausselim (elimcol (delrow (p, rows)))
  in    
    ((p::prow)::g_rows, if samerow then odd else ~odd)
  end;

fun diagprod ([], e: real)       = e
  | diagprod((x::xs)::rows, e) = diagprod (rows, x*e);

fun det rows = diagprod(signed_gausselim rows);

(* 3.27: See online solutions for a much more concise alternative. *)
(* TODO: Redo better *)
(* Create identity matrix to append onto the right. *)
fun matid size =
  let fun id (res, 0)          = res
        | id ([], n)           = id ([[1.0]], n-1)
        (* All rows will be non-empty, hence inexhaustive patterns. *)
        | id (((x::xs)::y), n) =
          id ((1.0::0.0::xs)::cons(0.0,(x::xs)::y), n-1)
  in id ([], size) end;

(* Map multiply by x over list *)
fun mult (n, [])    = [] : real list
  | mult (n, x::xs) = n*x::mult(n,xs);

(* Assert that ys only has one element by the end of it *)
fun addrows (x::xs, [y], 1)   = mult(~x,y)
  | addrows (x::xs, y::ys, n) =
      vectorsum(mult(~x, y), addrows(xs, ys, n-1));

(* Modified solutions function for matrix inversion. *)
fun back_sub [x::xs]         = [1.0::mult(1.0/x, xs)]
  | back_sub ((x::xs)::rows) =
  let
    val solved = cons(0.0, back_sub rows)
    (* Multiply each row by corresponding value in current row *)
    val rest   = addrows (xs, solved, length solved)
    (* Add sum of previous to current row then normalise *)
    val n::new_row = vectorsum(x::xs, rest)
  in
    (1.0::mult(1.0/n, new_row))::solved
  end;

(* Drop the identity matrix now on the left. *)
fun extract_sol rows =
  let
    val size = length rows
    fun droprows []      = []
      | droprows (x::xs) = List.drop (x, size) :: droprows xs
  in
    droprows rows
  end;

(* Finally, inverse matrix *)
fun matinverse rows =
(* Inexhaustive pattern matching so that we know when matrices are
 * not of the same dimensions. *)
  let fun app ([], [])       = []
        | app (x::xs, y::ys) = (x@y)::app(xs,ys)
  in
    extract_sol (back_sub (gausselim (app (rows, matid (length rows)))))
  end;

(* Online solution, to study. *)
fun zeroes 0 = []                        (* Simple enough function. *)
  | zeroes n = 0.0 :: zeroes(n-1);

(* Solving the matrix a column at a time:
 *          (a  b  c | p )
 *          (0  d  e | q )
 *          (0  0  f | r )
 * by isolating appropriate column through id matrix dot and then
 * storing values for ~r/f and ~(q-e/r)/d and so on. *)
fun rsolutions (endrow, [])            = endrow
  | rsolutions (endrow, (x::xs)::rows) =
      let val solns = rsolutions(endrow,rows)
      in ~(dotprod(solns,xs)/x) :: solns end;

fun inverse rows =
  let val n = length rows
      (* To generate id matrix on the side. *)
      fun idrow(x,k) = zeroes(k-1) @ [x] @ zeroes(n-k)
      fun newrows ([], k)        = []
        | newrows (row::rows, k) =
              (row @ idrow(1.0,k)) :: newrows(rows, k+1)
      (* Solve as normal. *)
      val ge = gausselim (newrows(rows,1))
      (* Major difference: solution built up in columns. *)
      fun newcols k =
            if k>n then []
            else List.take(rsolutions (idrow(~1.0,k), ge), n) :: newcols(k+1)
  in  transp2 (newcols 1)  end;

(* 3.28: A simple solution would be to have a boolean 'first_call' which is
 * true for the initial call of any function and false for any subsequent
 * recursive ones. That way, we can tell if a [] matrix represents a zero or
 * mismatched matrix dimensions.
 * Alternatively, we can check the dimensions of a matrix thoroughly each time
 * before performing any operations on it. *)
