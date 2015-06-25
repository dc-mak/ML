(* 2014, Paper 1, Question 2c *)

exception Empty;

fun max x y = if (x >= y) then x else y;

fun lcsLength xs ys =
let
  fun lcs [] _ = 0
    | lcs _ [] = 0
    | lcs (x::xs) (y::ys) =
    if x = y then
      (lcs xs ys) + 1
    else 
      max (lcs (x::xs) ys) (lcs xs (y::ys))
in
  lcs xs ys
end;

fun eqTest n xs ys = (lcsLength xs ys) <= n;

  (* And just for efficiency's sake and after
  * a little effort, obscurity's too...*)
fun dpLcsLength _ [] = raise Empty
  | dpLcsLength [] _ = raise Empty
  | dpLcsLength xs ys =
  let
    val m = List.length xs
    val n = List.length ys
    (* To iterate through, inner loop *)
    val count = List.tabulate(n+1, fn i => i)
    (* Each A(i,0) = 0 *)
    fun gen prev curr i 0 = 0
      (* point up-left, up or left *)
      | gen prev curr i j = 
      (* Evaluate element equivalence, then point *)
      if (List.nth(xs, i-1) = List.nth(ys, j-1)) then 
        List.nth(prev, j-1) + 1
      else 
        max (List.nth(prev, j)) (List.nth(curr, j-1))

    fun lcs prev 0 = prev
      | lcs prev i =
      (* From each column in the previous row, generate next *)
      lcs (map (fn j => gen prev prev (m+1-i) j) count) (i-1)
  in
    (* Last element of last row of LCS algorithm *)
    List.nth(lcs (List.tabulate(n+1, fn i => 0)) m, n-1)
  end;

fun dpEqTest n xs ys = (dpLcsLength xs ys) <= n;

dpLcsLength [1,2,3,4,5,6] [1,2,3];
dpLcsLength [1] [6,5,4,3,2,1];

(* DP Reconstruction *)
datatype arrow = U | D | L | R | UL | UR | DL | DR | H; (* H = Here *)

exception FalseAssertion;
exception WrongArrow;

fun dpLCS _ [] = raise Empty
  | dpLCS [] _ = raise Empty
  | dpLCS xs ys =
  let
    val m = List.length xs (* rows *)
    val n = List.length ys (* columns *)

    (* To iterate through, inner loop *)
    val count = List.tabulate(n+1, fn i => i)

    (* Start board of all empty places *)
    val start = List.tabulate(1, fn i => List.tabulate(n+1, fn i => (0, H)))

    (* Generate a row from previous *)
    (* Can probably generate this in reverse 
     * to avoid reversing each list later *)
    fun gen prev curr i 0 = (0, H)
      | gen prev curr i j = 
      let
        val xi = List.nth(xs, i-1) 
        val yj = List.nth(ys, j-1)
        val upLeft = #1(List.nth(prev, j-1))
        val up = #1(List.nth(prev, j))
        val left = #1(List.nth(curr, j-1))
      in
        if xi = yj then 
          (upLeft + 1, UL)
        else 
          if up >= left then
            (up, U)
          else
            (left, L)
      end

    (* From each column in the previous row, generate next *)
    fun lcs rows 0 = rows
      | lcs (rows as r::rs) i =
      let
        val next = map (fn j => gen r r (m+1-i) j) count
      in
        lcs (next::rows) (i-1)
      end
      | lcs _  _ = raise Empty


    (* Cycle through rows keeping track of row and column *)
    fun build row i j ans =
    let
      (* xs = [x0 ... x(m-1)] *)
      fun bld j' [] = raise Empty
        | bld j' (r::rs) = 
        case #2(r) of
             UL   => (j'+1, List.nth(xs, i-1)::ans)
           | U    => (j', ans)
           | L    => bld (j'+1) rs
           | H    => if i = 1 andalso j' = n then (j', ans)
                     else raise FalseAssertion
           | _    => raise WrongArrow
      val (j1, a1) = bld j (List.drop(row, j))
      in
        (i-1, j1, a1)
      end

      (* Add to solution one row at time *)
    fun analyse grid =
    let
      fun iter []         _               = raise Empty
        (* Last row has zeros *)
        | iter [r]        (row, col, ans) = (ans, List.length(ans))
        | iter (r::rs)    (row, col, ans) = iter rs (build r row col ans)
    in
      iter grid (m, 0, [])
    end

    (* Because column is IRRELEVANT
    *  Can be outside let..in..end *)
    fun flip grid = map (fn row => rev row) grid

    in
      (analyse o flip) (lcs start m)
    end;

dpLCS [1,2,3,5,6,3,4,1,4] [1,4,2,1,2,3,5,1,6,4,5];
dpLCS [1,5,1,4,51,5,1,3,3,2,1] [1,2,2,3,5,1,3,5,1,2];
