(* 3.29: nextperm [2,3,1,4] = next ([2], [3,1,4])
 *                      => next ([3,2], [1,4])
 *                      => swap [3,2]
 *                      => 3::swap [2]
 *                      => 3::1::2::[4]
 *                      => [3,1,2,4]            *)

(* 3.30: If we look at [2,2,3,1] then replace <= with < would cause a loop. *)

(* 3.31: Match or empty (probably match because of the pattern matching in the
 * function clause. Add this clause - next (xs, []) = xs. *)

(* 3.32: 1 for the first and 500 for the second. *)

(* 3.33: I think the second one is more efficient because it requires constant
 * stack space since its recursive calls are iterative, despite number of
 * recursive calls being the same. *)

(* 3.34: *)
fun cons (x, [])    = []
  | cons (x, y::ys) = (x::y)::cons(x,ys);

fun choose (0, xs)    = [[]]
  | choose (k, [])    = []
  | choose (k, x::xs) =
    cons(x, choose(k-1, xs)) @ choose (k, xs);

(* 3.35: The functions differ in only when the recursive call of
 * cprod (xs, ys) is made. For the second function, it may uses more stack
 * space since everything is calculated in one very deep recursion.
 * Additional: if m = len xs and n = len ys then cartprod takes m+n
 * *nested* calls but cprod takes m*n *nested* calls. *)

(* 3.36: Corrected with online solution. *)
use "working-programmer/examples/sample3.sml";

fun pathsort graph =
  let
    fun sort ([], path, visited)    = [visited]
      | sort (x::xs, path, visited) =
      if x mem path then []
      else sort (xs, path,
        if x mem visited then visited
        else
          let fun prop [] = []
                | prop [xs] = [x::xs]
          in prop (sort(nexts(x,graph), x::path, visited)) end)
    val (starts, _) = ListPair.unzip graph
  in
    sort (starts, [], [])
  end;

(* 3.37: Yes and it'll contain all nodes? (Yes.) *) 

(* 3.38: *)
fun quicker ([], sorted)    = sorted
  | quicker (a::bs, sorted) =
  let fun part (left, right, [])    =
            quicker (left, a::quicker (right,sorted))
        | part (left, right, x::xs) =
          if x <= a then part (x::left, right, xs)
                    else part (left, x::right, xs)
  in part ([], [], bs) end;

(* 3.39: This is meant to start from 1, online solutions' starts from 0. *)
fun find (a::bs, i) =
    let fun part (left, right, []) =
        let val len = length left in
        if len+1 < i then find (right, i-1-len) else
        if len+1 > i then find (left, i) else a end
         | part (left, right, x::xs) =
        if x <= a then part (x::left, right, xs)
                  else part (left, x::right, xs)
    in part ([], [], bs) end;
 
(* 3.40: This starts from 1, online solutions' starts from 0. Online one
 * is more concise but sacrifices a little clarity. *)
fun findrange ([], i, j)    = []
  | findrange (a::bs, i, j) =
    let fun part (left, right, []) =
        let
          val len = length left
        in
          if i <= len andalso j <= len then
            findrange (left, i, j)
          else if i <= len andalso  j > len then
            findrange (left, i, len) @ [a] @ findrange (right, 1, j-len-1)
          else if i = len + 1 andalso j > len then
            a :: findrange (right, 1, j-len-1)
          else (* i > len + 1 andalso j > len + 1 *)
            findrange (right, i-len-1, j-len-1)
        end
         | part (left, right, x::xs) =
           if x <= a then part (x::left, right, xs)
                     else part (left, x::right, xs)
    in if j < i then [] else part ([], [], bs) end;

(* 3.41: *)
fun generate_list () = randlist (10000000, 1.0, []);

fun alts ([], xs, ys)       = (xs, ys)
  | alts ([x], xs, ys)      = (x::xs, ys)
  | alts (x::y::l, xs, ys)  = alts (l, x::xs, y::ys);

fun tmergesort'' []   = []
  | tmergesort'' [x]  = [x]
  | tmergesort'' xs   =
    let val (half, other_half) = alts (xs, [], [])
    in  merge (tmergesort'' half, tmergesort'' other_half) end;

fun time() =
  let
    val (seed, rs)  = generate_list()
    val cPU_time    = Timer.startCPUTimer()
    and real_time   = Timer.startRealTimer()
    val sorted      = tmergesort'' rs
  in
    (Timer.checkCPUTimer cPU_time, Timer.checkRealTimer real_time)
  end;

(* 3.42: *)
fun takedrop ([], n, xs)    = (xs, [])
  | takedrop (x::l, n, xs)  =
    if n > 0 then takedrop (l, n-1, x::xs) else (xs, x::l);

fun tmergesort''' []   = []
  | tmergesort''' [x]  = [x]
  | tmergesort''' xs   =
    let val (half, other_half) = takedrop (xs, length xs div 2, [])
    in  merge (tmergesort''' half, tmergesort''' other_half) end;

fun time'() =
  let
    val (seed, rs)  = generate_list()
    val cPU_time    = Timer.startCPUTimer()
    and real_time   = Timer.startRealTimer()
    val sorted      = tmergesort''' rs
  in
    (Timer.checkCPUTimer cPU_time, Timer.checkRealTimer real_time)
  end;

(* 3.43: So that the lists are not immediately merged but are sorted first. *)

(* 3.44: This detects increasing run before a decreasing. *)
fun incrun (run, [])    = (rev run, [] : real list)
  | incrun (run, x::xs) =
    if x < hd run then decrun ([x], rev run, xs)
                  else incrun (x::run, xs)
and decrun (dec, inc, [])       = (merge (dec, inc), [])
  | decrun (dec, inc, x::xs)    =
    if x > hd dec then incrun ([x], xs@merge (dec, inc))
                  else decrun (x::dec, inc, xs);

(* After looking structure of online solution briefly. Would be much more
 * concise by passing the comparison operator as a higher order function. *)
fun nextdec (run, [])    = (run, [] : real list)
  | nextdec (run, x::xs) =
    if x > hd run then (run, x::xs)
                  else nextdec (x::run, xs);

(* Close, minor adjustments made in correction. *)
fun samsorting' ([],    ls, k)  = hd (mergepairs (ls, 0))
  | samsorting' ([x],   ls, k)  = hd (mergepairs ([x]::ls, 0))
  | samsorting' (x::y::xs, ls, k)  =
  let val (run, tail) = if x < y then nextdec ([y,x], xs)
                                 else nextrun ([y,x], xs)
  in samsorting' (tail, mergepairs (run::ls, k+1), k+1) end;

fun samsort' xs = samsorting' (xs, [[]], 0);

(* 3.45: The same as sum, but with + replaced by - and the first clause's
 * result negated with termprod ((0, ~1.), us). *)

(* 3.46: *)
fun show ts =
  let
    fun term (m,a) =
    let val pow = Int.toString m
        val coeff = Real.toString a
    in if m = 0 then coeff else
       if m = 1 then coeff^"x"
                  else coeff^"x"^pow
    end
    
    fun s ([]         , str) = str
      | s ([t]  , str) = term t
      | s (t::ts, str) = s (ts, str^term t^" ")
  in s (ts, "") end;

(* 3.47: prod uses sum so because sum ensures coefficients are returned in
 * decreasing powers of x due to the if conditions and a zero coefficent 
 * means the term is not stored, they respect the representation.
 * Correction: need to mention take and drop in prod also maintain valid
 * polynomials. *)

(* 3.48: Define the sum of two polynomials. Show that the function terminates.
 * Prove bases cases. Assume correct inputs for non-base cases.
 * Show that each term consed to the list maintains invariants relating to
 * polynomial sums.
 * Correction: could go abstract with sets and reduce to union proof. *)
