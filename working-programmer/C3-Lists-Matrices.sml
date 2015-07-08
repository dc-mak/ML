(* 3.19: This one - []::row and it'll return the transpose of a matrix
 * of the columns whose max size will be the min size of the rows.
 * Checked: the second. *)

(* 3.20: Raise match or that that free variable thing.
 * Correction: it *loops* because both headcols and tailcols accept empty
 * lists as arguments which are then returned and recursed till stack overflow. *)

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

use "working-programmer/examples/sample3-matrices.sml";

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
