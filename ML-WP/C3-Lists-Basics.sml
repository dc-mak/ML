(* 3.1 *)
fun null []     = true
  | null (_::_) = false;

fun maxl xs =
  let
    fun hdtl ys = (hd xs, tl xs)
    val (m, ms) = hdtl xs
    val (n, ns) = hdtl ms
  in
    if null ms then m
               else if m > n then maxl (m::ns)
                             else maxl (n::ns)
  end;

(* 3.2 *)
fun lst [m]     = m
  | lst (_::ms) = lst ms;

local
  fun addlen (n, [])   = n
    | addlen (n, x::l) = addlen (n+1, l)
in
  fun length l = addlen (0, l)
end;

(* 3.3: They'd raise an exception Match/return the list as is, respectively. *)

(* 3.4 *)
fun nth (m::l, n) = if n <= 0 then m else nth (l, n-1);
(* or online solution: fun nth (l,n) = hd (drop (l,n)) *)

(* 3.5 *)
infixr 5 @;
fun [] @ ys = ys
  | xs @ [] = xs
  | (x::xs) @ ys = x::(xs @ ys);

(* 3.6: Type error because x is not a list but [x] is?
 *       Correction: nrev = fn : 'a list list -> 'a list
 *       so nrev [[1],[2],[3],[4]] = [4,3,2,1].  *)

(* 3.7:
 * nrev [1,2,3,4]   => nrev [2,3,4] @ [1]
 *                  => (nrev [3,4] @ [2]) @ [1]
 *                  => (nrev [4] @ [3]) @ [2]) @ [1]
 *                  => (([4] @ [3]) @ [2]) @ [1]
 *                  => ((4::([]@[3]) @ [2]) @ [1]
 *                  => ((4::[3]) @ [2]) @ [1]
 *                  => ([4,3] @ [2]) @ [1]
 *                  => (4::([3] @ [2])) @ [1]
 *                  => (4::3::([] @ [2])) @ [1]
 *                  => (4::3::[2]) @ [1]
 *                  => (4::[3,2]) @ [1]
 *                  => [4,3,2] @ [1]
 *                  => 4::([3,2] @ [1])
 *                  => 4::3::([2] @ [1])
 *                  => 4::3::2::([] @ [1])
 *                  => 4::3::2::[1]
 *                  => 4::3::[2,1]
 *                  => 4::[3,2,1]
 *                  => [4,3,2,1]
 *
 * rev [1,2,3,4]    => revAppend (1::[2,3,4], [])
 *                  => revAppend (2::[3,4], 1::[])
 *                  => revAppend (3::[4], 2::[1])
 *                  => revAppend (4::[], 3::[2,1])
 *                  => revAppend ([], 4::[3,2,1])
 *                  => [4,3,2,1].
 *)

(* 3.8: Should be about the same efficiency, maybe slightly better by
 * pattern-matching out any empty lists except the last one?
 * Correction: Uses *one deep recursion* so may cause stack overflow. *)

 (* 3.9 *)
fun zip (_, [])        = []
  | zip ([], _)        = []
  | zip (x::xs, y::ys) = (x,y)::zip (xs, ys);

(* 3.10: Same order of magnitude?
 * Correction: rev (rtake (i, l, [])) performs twice as many :: operations
 * (once for building list in reverse, once for reversing) but only requires
 * CONSTANT stack space.
 * In take(i,l) requires stack proportional to the length of result.
 *          If enough stack space, take (i,l) is faster.
 *)

(* 3.11: TODO Make lazy version of this to check it.
 *       Didn't know that only one way, not all ways required.
 *       See C3-RomNum.sml for a more detailed attempt. *)
local
  val numerals = [(1000, #"M"), (500, #"D"), (100, #"C"),
                  (50, #"L"), (10, #"X"), (5, #"V"), (1, #"I")]
  fun revList [] = []
    | revList (x::xs) = implode (rev x)::revList xs;

  fun toRom (sofar, vals,      0) = [sofar]
    | toRom (sofar, [],      amt) = []
    | toRom (sofar, [(v,c)], amt) =
        if amt < 0 then []
        else toRom (c::sofar, [(v,c)], amt-v)
    | toRom (sofar, (v, c)::(v', c')::vals, amt) =
        if amt < 0 then []
        else toRom (c::sofar, (v,c)::(v',c')::vals, amt-v) @
             toRom (sofar, (v',c')::vals, amt) @
             toRom (c::c'::sofar, vals, amt+v'-v)
in
  fun roman n = revList (toRom ([], numerals, n))
end;

(* Extension of 3.11: quite untidy. *)
local
  fun toRom (sofar, vals, 0,   skip) = [sofar]
    | toRom (sofar, vals, amt, skip) =
    if amt < 0 then []
    else case vals of
              [] => []
            | [(v,c)] =>
                (* Allow only 3 repetitions for smallest value *)
                toRom (c::sofar, [], amt-v, false) @
                toRom (c::c::sofar, [], amt-2*v, false) @
                toRom (c::c::c::sofar, [], amt-3*v, false)
            | ((v,c)::(v',c')::vs) =>
                (* No reps/skip this value *)
                toRom (sofar, (v',c')::vs, amt, false) @

                (* One before such as {C,L,X,V,}M for respective values. *)
                List.concat(map
                  (fn (v2,c2) => toRom (c::c2::sofar, vs, amt+v2-v, false))
                (* If to prevent LC = L or VX=X and DM = D. *)
                (if (c = #"X" orelse c = #"C" orelse c = #"M") then vs
                 else ((v',c')::vs))) @

                (* One rep and avoiding duplicates *)
                (if (c = #"X" orelse c = #"C" orelse c = #"M") then []
                 else toRom (c::sofar, (v',c')::vs, amt-v, false)) @

                (* Two reps excluding these because X=VV, C=LL, M=DD. *)
                (if (c = #"V" orelse c = #"L" orelse c = #"D" orelse skip)
                 then [] else toRom (c::c::sofar, (v',c')::vs, amt-2*v, false) @
                      (* This bit is to allow XXIX or CC{X,V,I}C *)
                      toRom (c::c::sofar, (v,c)::(v',c')::vs, amt-2*v, true) @
                      (* Three reps *)
                      toRom (c::c::c::sofar, (v',c')::vs, amt-3*v, false)) @

                (* Unlimited reps for M *)
                (if c <> #"M" then []
                 else toRom (c::sofar, (v,c)::(v',c')::vs, amt-v, false))

  val numerals = [(1000, #"M"), (500, #"D"), (100, #"C"),
                  (50, #"L"), (10, #"X"), (5, #"V"), (1, #"I")]
  fun revList [] = []
    | revList (x::xs) = implode (rev x)::revList xs;

in
  fun roman n = revList (toRom ([], numerals, n, false))
end;

(* For later: roman 829 (and some other values) cause duplicates. *)
(* Online solution *)
val rompairs1 = [("M",1000), ("D",500), ("C",100),
                 ("L",50), ("X",10), ("V",5), ("I",1)]
and rompairs2 = [("M",1000), ("CM",900), ("D",500), ("CD",400),
                 ("C",100),  ("XC",90),  ("L",50),  ("XL",40),
                 ("X",10),   ("IX",9),   ("V",5),   ("IV",4), ("I",1)];

fun toRom (vals, 0)          = ""
  | toRom ([],   _)          = ""
  | toRom ((s,v)::vals, amt) =
      if amt < v then toRom (vals, amt)
      else s^toRom ((s,v)::vals, amt-v);

(* 3.12: Should work with longest subseq. of elements in decreasing order. *)

(* 3.13: Given a list of (number, value) pairs for coins... *)
fun allChange (coins, vals, 0)             = [coins]
  | allChange (coins, [], amt)             = []
  | allChange (coins, (0, v)::vals, amt)   = allChange (coins, vals, amt)
  | allChange (coins, (num, v)::vals, amt) =
      if amt < 0 then []
      else allChange (v::coins, (num-1, v)::vals, amt-v) @
           allChange (coins, vals, amt);

(* 3.14: Tada, (from what I remember in the notes...) *)
fun allC (coins, vals, amt) =
let
  fun change (coins, vals, 0, result)      = coins::result
    | change  (coins, [], amt, result)     = result
    | change (coins, v::vals, amt, result) =
        if amt < 0 then result
        else change (coins, vals, amt, change (v::coins, v::vals, amt-v, result))
in
  change (coins, vals, amt, [[]])
end;
