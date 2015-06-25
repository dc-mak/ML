(* 3.1 *)
fun null [] = true
  | null (_::_) = false;

fun hd (x::_) = x;
fun tl (_::xs) = xs;

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
fun lst [m] = m
  | lst (_::ms) = lst ms;

local 
  fun addlen (n, []) = n
    | addlen (n, x::l) = addlen (n+1, l)
in
  fun length l = addlen (0, l)
end;

(* 3.3: They'd raise an exception Match/return the list as is, respectively. *)

(* 3.4 *)
fun nth (m::l, n) = if n <= 0 then m else nth (l, n-1);
(* or this: fun nth ln = hd (drop ln) *)

(* 3.5 *)
infixr 5 @;
fun [] @ ys = ys
  | xs @ [] = xs
  | (x::xs) @ ys = x::(xs @ ys);

(* 3.6: Type error because x is not a list but [x] is?
 *       Not quite: nrev = fn : 'a list list -> 'a list
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
 * Uses *one deep recursion* so may cause stack overflow. *)

 (* 3.9 *)
fun zip (_, []) = []
  | zip ([], _) = []
  | zip (x::xs, y::ys) = (x,y)::zip (xs, ys);

(* 3.10: Same order of magnitude?
 * Nah mate, rev (rtake (i, l, [])) performs twice as many :: operations 
 * (once for building list in reverse, once for reversing) but only requires
 * CONSTANT stack space.
 * In take(i,l) requires stack proportional to the length of result.
 *          If enough stack space, take (i,l) is faster.
 *)

(* 3.11: TODO Make lazy version of this to check it.
 *       Didn't know that only one way, not all ways required. *)
local 
  val numerals = [(1000, #"M"), (500, #"D"), (100, #"C"),
                  (50, #"L"), (10, #"X"), (5, #"V"), (1, #"I")]
  fun revList [] = []
    | revList (x::xs) = implode (rev x)::revList xs;

  fun toRom (sofar, vals, 0) = [sofar]
    | toRom (sofar, [], amt) = []
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

(* Well, Larry did this. *)
val rompairs1 = [("M",1000), ("D",500), ("C",100),
                 ("L",50), ("X",10), ("V",5), ("I",1)]
and rompairs2 = [("M",1000), ("CM",900), ("D",500), ("CD",400), 
                 ("C",100),  ("XC",90),  ("L",50),  ("XL",40), 
                 ("X",10),   ("IX",9),   ("V",5),   ("IV",4), ("I",1)];

fun toRom (vals, 0) = ""
  | toRom ([], _) = ""
  | toRom ((s,v)::vals, amt) =
  if amt < v then toRom (vals, amt)
  else s^toRom ((s,v)::vals, amt-v);

(* 3.12: Should work with longest subseq. of elements in decreasing order. *)

(* 3.13: Given a list of (number, value) pairs for coins... *)
fun allChange (coins, vals, 0) = [coins]
  | allChange (coins, [], amt) = []
  | allChange (coins, (0, v)::vals, amt) = allChange (coins, vals, amt)
  | allChange (coins, (num, v)::vals, amt) =
  if amt < 0 then []
  else allChange (v::coins, (num-1, v)::vals, amt-v) @
       allChange (coins, vals, amt);

(* 3.14: Tada, (from what I remember in the notes...) *)
fun allC (coins, vals, amt) =
let
  fun change (coins, vals, 0, result) = coins::result
    | change  (coins, [], amt, result) = result
    | change (coins, v::vals, amt, result) =
    if amt < 0 then result
    else change (coins, vals, amt, change (v::coins, v::vals, amt-v, result))
in
  change (coins, vals, amt, [[]])
end;

(* 3.15 *)
fun bincarry (false, ps) = ps
  | bincarry (true, []) = [true]
  | bincarry (true, p::ps) = not p :: bincarry (p, ps);


fun binsum (c, [], qs) = bincarry (c, qs)
  | binsum (c, ps, []) = bincarry (c, ps)
  | binsum (c, p::ps, q::qs) =
  let
    (* Larry: At least two bits are true *)
    fun carry (a,b,c) = a andalso b orelse a andalso c orelse b andalso c;
    (* Larry: b=c is equivalent to not (b XOR c) so... *)
    fun sum   (a,b,c) = (a=(b=c));

    (* What I did...
    fun xor (a, b) = a andalso not b orelse not a andalso b
    fun sum (a, b, c) = xor(xor(a,b),c)
    fun carry (a, b, c) = c andalso (a orelse b) orelse a andalso b *)
  in
    sum (p, q, c) :: binsum (carry (p, q, c), ps, qs)
  end;

fun binprod ([], _) = []
  | binprod (false::ps, qs) = false::binprod (ps, qs)
  | binprod (true::ps, qs) = binsum (false, qs, false::binprod (ps, qs));

(* 3.16: See the structure... This is all Larry. *)
(*
signature ARITH =
    sig
    type t
    val zero : t
    val sum  : t * t -> t
    val diff : t * t -> t
    val prod : t * t -> t
    val quo  : t * t -> t
    end;
*)
    
(* 3.17: TODO Study and understand thoroughly. Redo 1995.1.6 *)
structure Bin = (* ARITH = *)
struct
  type  t = int list
  
  val zero = []
  
  (* Equivalent to add 1 *)
  fun carry (0, ps) = ps
    | carry (1, []) = [1]
    | carry (1, p::ps) = (1-p) :: carry (p, ps);

  (* Sum logic and carry logic propogated through list *)
  fun sumc (c, [], qs) = carry (c,qs)
    | sumc (c, ps, []) = carry (c,ps)
    | sumc (c, p::ps, q::qs) =
        ((c+p+q) mod 2)::sumc((c+p+q) div 2, ps, qs);

  (* Start with 0 carry *)
  fun sum (ps,qs) = sumc (0,ps,qs);

  (* Skip for 0, double for 1 and add recursively *)
  fun prod ([], _) = []
    | prod (0::ps, qs) = 0::prod(ps,qs)
    | prod (1::ps, qs) = sum(qs, 0::prod(ps,qs));

  (** Subtraction **)
  infix $$;
  (* This is beautiful. Think of it like a special cons. *)
  fun 0 $$ [] = []          (* Gets rid of any leading 0s *)
    | n $$ [~1] = [~1]      (* Propogate/signal that result *)
    | n $$ ns = n::ns;      (* is negative. Otherwise, cons *)

  (* Equivalent to subtract one *)
  fun borrow (0, ps) = ps           (* Nothing to borrow *)
    | borrow (~1, []) = [~1]        (* Result is negative *)
    (* Flip bit: 1-1=0, 0-1=10-1=1 and subtract 1 from next *)
    | borrow (~1, p::ps) = (1-p) $$ borrow (p-1, ps);

  (* Invariants: b=0 or ~1.
   *        b+p-q = 1, 0, -1, -2
   *        mod 2 = 1, 0,  1,  0
   *        div 2 = 0, 0, -1, -1 *)
  fun diffb (b, ps, []) = borrow (b,ps)
    (* To catch leading 0s, I think *)
    | diffb (b, [], q::qs) =    
        ((b-q) mod 2) $$ diffb ((b-q) div 2, [], qs)
    (* Standard bit-by-bit subtraction with borrow value. Write out
     * "truth" table of (b_in, x, y) -> (b_out, sum) to check. *)
    | diffb (b, p::ps, q::qs) =
        ((b+p-q) mod 2) $$ diffb ((b+p-q) div 2, ps, qs);

  (* Start with 0 borrow *)
  fun diff (ps,qs) = diffb (0,ps,qs);

  (** Division **)
  fun divide (ps,ds,n,qs) =
    if n=0 then (qs,ps)
    else
      let val rs = diff (ps,ds)
      in  if rs = [~1] then divide(0::ps, ds, n-1, 0::qs)
                       else divide(0::rs, ds, n-1, 1::qs)
      end;

  (*Scan down list counting bits in k; get position of last "1" (in n)*)
  fun lastone (n,k,[]) = n
    | lastone (n,k,0::ps) = lastone(n,k+1,ps)
    | lastone (n,k,1::ps) = lastone(k,k+1,ps);

  (* I was somewhat in the ballpark with the 2's complement one. *)
  fun addzeros (0,ds) = ds
    | addzeros (k,ds) = 0::addzeros(k-1,ds);

  fun quorem (ps,ds) =
    let val n = lastone(0,1,ps) - lastone(0,1,ds)
    in  if n<0 then ([0],ps)
        else let val (qs,rs) = divide(ps, addzeros(n,ds), n+1, [])
             in  if length rs < n+1 then (qs,rs)
                 else (qs, List.drop(rs,n+1))
             end
    end;

  fun quo (ps,qs) = #1(quorem(ps,qs))
  and rem (ps,qs) = #2(quorem(ps,qs));

  fun gcd(ms,ns) =
      if lastone(0,1,ms)=0 then  ns  else  gcd(rem(ns,ms), ms);
end;
