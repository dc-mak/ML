signature MATRIX =
  sig
  include ARITH
  structure Num : NUMBER
  val inv    : t -> t
  val transp : t -> t
  val det    : t -> Num.t
  end;

functor Matrix (Num : NUMBER) :> MATRIX where type t' = Num.t list list =
  struct
  (* Auxilliary *)
  structure Num = Num
  val ~   = Num.neg
  and op+ = Num.sum
  and op- = Num.diff
  and op* = Num.prod
  and op/ = Num.divd

  infix ==
  fun x == y = Num.eq (x,y)
  fun x >= y = Num.compare (x,y) <> LESS

  (* Dimensions check *)
  fun check (_, [])        = true
    | check (n, [x])       = n = length x orelse n = ~1
    | check (~1, x::y::ys) = let val n = length y in
                             length x = n andalso check (n, ys) end
    | check (n, y::ys)     = let val m = length y in
                             m=n andalso check (n,ys) end

  exception Matrix_Operation
  exception Dimensions

  fun dim xs = if check (~1, xs) then xs else raise Dimensions

 (* Signature related *)
  type t'    = Num.t list list
  datatype t = Mat of t'
  val zero   = Mat []

  fun make rows = Mat (dim rows)
  fun dest (Mat rows) = rows
  
  (* Not fancy, but factors to take into account:
   *    # very long ints in Rat.t
   *    # very long ints/reals in Complex.t
   *    # variable formatting (0.xx, E) for reals
   *    # centering, tabulation, ascii art, etc. *)
  fun toString (Mat rows) =
    let fun drop str        = String.extract (str, 2, NONE)
        fun frmt (num, row) = row^", "^Num.toString num
        fun prnt (row, mat) = mat^"\n[ "^drop (foldl frmt "" row)^" ]"
    in  "[ "^drop (foldl prnt "" rows)^" ]\n" end

  fun neg (Mat rows) = Mat (map (fn row => map ~ row) rows)

  (* Pesky zero matrix cases *)
  fun sum (Mat [], ys)     = ys
    | sum (xs, Mat [])     = xs
    | sum (Mat xs, Mat ys) =
        Mat (ListPair.mapEq (fn xy => ListPair.mapEq op+ xy) (xs,ys))

  (* Pesky zero matrix cases *)
  fun diff (Mat [], ys) = neg ys
    | diff (xs, Mat []) = xs
    | diff (Mat xs, Mat ys) =
        Mat (ListPair.mapEq (fn xy => ListPair.mapEq op- xy) (xs,ys))

  (* fun trans []         = []
       | trans ([]::rows) = []
       | trans rows       = (map hd rows)::trans (map tl rows)      *)
  (* This one is faster I think (see working-programmer/Transp.sml) *)
  fun trans []                    = []
    | trans (r::rows)             =
      let fun join ([], ys)       = []
            | join (xs, [])       = map (fn x => [x]) xs
            | join (x::xs, y::ys) = (x::y)::join(xs,ys)
      in  join (r, trans rows) end

  fun transp (Mat [])        = Mat []
    | transp (Mat (r::rows)) = Mat (trans rows)

  (* And the work begins... helps with product and inversion. *)
  val dotprod = ListPair.foldlEq (fn(x,y,e)=>x*y+e) Num.zero

  fun prod (Mat [], ys)      = Mat []
    | prod (xs, Mat [])      = Mat []
    | prod (Mat xs, Mat ys)  =
    let fun rowprod cols row = map (fn col => dotprod (row,col)) cols
    in  Mat (map (rowprod (trans ys)) xs) end

  (* More useful for floating point than rationals. *)
  fun pivotrow []    = []
    | pivotrow [row] = row
    | pivotrow (row1::row2::rows) =
        if Num.abs (hd row1) >= Num.abs (hd row2)
          then pivotrow(row1::rows) else pivotrow(row2::rows)

  (* The matrix excluding the first row with head p *)
  fun delrow (p, []) = []
    | delrow (p, r::rows) =
        if p == hd r then rows else r::delrow(p, rows)

  fun scalarprod (k, xs) = map (fn x => k*x) xs
  val vectorsum          = ListPair.mapEq op+

  fun signed_gausselim []     = ([],    Num.zero)
    | signed_gausselim [row]  = ([row], Num.one)
    | signed_gausselim rows   =
      let
        val (p,prow) = case pivotrow rows of
                             []      => raise Matrix_Operation
                           | (r::rs) => (r, rs)

        val samerow = Num.abs (hd (hd rows)) == Num.abs p

        fun elimcol []              = []
          | elimcol ([]::_)         = raise Dimensions
          | elimcol ((x::xs)::rows) =
              vectorsum (xs, scalarprod(~x/p, prow))::elimcol rows

        val (g_rows, odd) = signed_gausselim (elimcol (delrow (p, rows)))
      in
        ((p::prow)::g_rows, if samerow then odd else ~odd)
      end

  fun diagprod (rows, r) = foldl (fn(x,e)=> e*hd x) r rows
  fun det (Mat rows)     = diagprod (signed_gausselim rows)

  (* Solving the matrix a column at a time:
   *          (a  b  c | p )
   *          (0  d  e | q )
   *          (0  0  f | r )
   * by isolating appropriate column through id matrix dot and then
   * storing values for ~r/f and ~(q-e/r)/d and so on. *)
  fun rsolutions (endrow, []::_)         = raise Dimensions
    | rsolutions (endrow, [])            = endrow
    | rsolutions (endrow, (x::xs)::rows) =
        let val solns = rsolutions(endrow,rows)
        in  ~(dotprod(solns,xs)/x) :: solns end

  fun inv (Mat rows) =
    let
      (* For this function only *)
      infix 6 ++
      val op++  = Int.+

      (* And the rest *)
      val l  = length rows
      and l' = length (hd rows)
      val n  = if l = l' then l else raise Dimensions

      fun idrow (x,k) = List.tabulate (n, fn i => if i=k then x else Num.zero)

      fun newrows ([], k)      = []
        | newrows (r::rows, k) = (r@idrow (Num.one, k))::newrows (rows, k++1)

      val ge = #1(signed_gausselim (newrows(rows,1)))

      fun newcols k = if k>n then [] else
            List.take (rsolutions (idrow (~ Num.one, k), ge), n)::newcols(k++1)
    in
      Mat (trans (newcols 1))
    end

  fun divd (a, b) = prod (a, inv b)

  (* Didn't use map for the laziness of andalso *)
  fun row_eq (x::xs, y::ys) = x == y andalso row_eq (xs,ys)
    | row_eq ([], [])       = true
    | row_eq _              = raise Dimensions

  fun eq (Mat (x::xs), Mat (y::ys)) = row_eq (x,y) andalso eq (Mat xs, Mat ys)
    | eq (Mat [], Mat [])           = true
    | eq _                          = raise Dimensions
  end;
