(* 8.1: False. *)

(* 8.2 *)
infix 3 +:=;
fun x +:= y = (x := !x + y);

(* 8.3: Returns unit, increments value at reference.
 *      Returns integer that double the value of that at reference q.
 *      Online solution: needs a space otherwise it thinks it's an identifier. *)

(* 8.4: For (E1; E2; ...; En) we can have
 *              let val _ = E1
 *                  val _ = E2
 *                  ...
 *              in  En end 
 *      and for while E1 do E2 we can have
 *      infix do 
 *      fun E1 do E1 = (E1, E2)
 *      fun while (E1, E2) =
 *          if E1 then let val _ = E2 in while (E1, E2) end else ()
 *      
 *      Online solution - The Definition of Standard ML states:
 *          case E1 of _ => case E2 of _ => ... case E(n-1) of _ => En
 *      and for while E1 do E2 it is
 *              let fun newVar() = if E1 then (E2; newVar()) else ()
 *              in  newVar()  end; *)

(* 8.5: *)
fun findroot (a, acc) =
    let
      fun nextx prev = (a/(!prev) + !prev)/2.0
      val prev = ref a
      val curr = ref (nextx prev)
    in
      while abs (!curr - !prev) >= acc * !prev do
        prev := (!curr before curr := nextx curr);
      !curr
    end;

(* 8.6: *)
fun fib n =
  let
    val loop = ref 0
    val prev = ref 0
    val curr = ref 1
  in
    while (!loop < n) do (
      prev := (!curr before curr := !curr + !prev);
      loop := !loop + 1);
    !prev
  end;

(* 8.7: *)
fun simul_assign (xrs, xs) = ListPair.app (op:=) (xrs, xs);

(* 8.8: It creates a new reference containing x and then returns its
 *      contents. The expression is legal because a new reference is being
 *      created at each point and the fun declaration gives it the 'a -> 'a
 *      type. *)

(* 8.9: val funs = [hd]     is fine because it is an ('a list -> 'a) list.
*       val l    = []       causes a free type variable error.
*       val l'   = tl [3]   is fine because [] : int list.
*
*       val lp   = let fun nilp x = ref [] in nilp () end
*                           would cause problems same as 2. *)

use "signatures/IMP_SEQUENCE.sig";
use "structures/ImpSeq.sml";

(* 8.10: *)
fun secr f y x = f (x,y);

fun merge (xq, yq) = 
  let val x = ImpSeq.hd xq
      and y = ImpSeq.hd yq
  in  if  x < y then ImpSeq.cons (x, fn()=> merge (ImpSeq.tl xq, yq)) else
      if  x > y then ImpSeq.cons (y, fn()=> merge (xq, ImpSeq.tl yq)) 
      else ImpSeq.cons (x, fn()=> merge (ImpSeq.tl xq, ImpSeq.tl yq))
  end;

val hamming = ImpSeq.cycle (fn hamf =>
    ImpSeq.cons (1, fn()=>
      merge (ImpSeq.map (secr op* 2) (hamf()),
             merge (ImpSeq.map (secr op* 3) (hamf()),
                    ImpSeq.map (secr op* 5) (hamf())))));

(* 8.11: *)
fun iterates f x = ImpSeq.cycle (fn iterf =>
    ImpSeq.cons (x, fn()=> ImpSeq.map f (iterf())));

(* 8.12: Apart from the fact the definition of cylce itself is confusing,
 * proving the correctness of a cyclic sequence is probably best done with
 * complete induction.
 * So following that idea, fib2 will keep looping on the tail sequence call
 * because there is only one element given. *)

(* 8.13: *)
fun toList (xf, 0) = []
  | toList (xf, n) = if n < 0 then raise Subscript else
    if ImpSeq.null xf then [] else ImpSeq.hd xf::toList (ImpSeq.tl xf, n-1);

fun fromList []      = ImpSeq.empty
  | fromList (x::xs) = ImpSeq.cons (x, fn()=> fromList xs);

fun interleave (xf, yf) =
    if ImpSeq.null xf then yf
    else ImpSeq.cons (ImpSeq.hd xf, fn()=> interleave (yf, ImpSeq.tl xf));

fun concat xqq = 
  if ImpSeq.null xqq then ImpSeq.empty else
  if ImpSeq.null (ImpSeq.hd xqq) then concat (ImpSeq.tl xqq) else
  let val x   = ImpSeq.hd (ImpSeq.hd xqq)
      val xq  = ImpSeq.tl (ImpSeq.hd xqq)
      val tlq = ImpSeq.tl xqq
  in ImpSeq.cons (x, fn()=> interleave (xq, concat tlq)) end;

fun filter pred xq =
  if ImpSeq.null xq then xq else 
  let val (x, tlq) = (ImpSeq.hd xq, ImpSeq.tl xq) in
  if pred x then ImpSeq.cons (x, fn()=> filter pred tlq)
            else filter pred tlq end;

(* 8.14:
fun delete (Ptr p) =
    case !p of
        Nil => raise Empty (* corrected from: false *)
      | Node(lp,x,rp) =>
           (if left(!lp) = lp then
               (p := Nil; true)
            else
              (right (!lp) := !rp;
               left (!rp)  := !lp;
               p           := !rp);
               false)                               *)

(* 8.15: The second one and the third ones would work?
 * The first one would also work but requires an equality type needlessly. *)

(* 8.16: The calls to ref are ommitted, since that is not a syntactic value,
 * the same reference is used meaning later updates would probably not work.
 * Online solution: yes, would not be able to create buffer larger than 1. *)

(* 8.17:
fun insert (Ptr p, x) =
    case !p of
    Nil => 
        let val lp = ref Nil
            and rp = ref Nil
            val new = Node(lp,x,rp)
        in  lp := new;  rp := new;  p := new  end
  | Node(_,_,rp) =>
        let val new = Node(ref(!p), x, ref(!rp))
        in  left (!rp) := new;  rp := new  end;     *)

(* 8.18: val buf = RingBuf.empty();
 *       RingBuf.insert(buf, "They");
 *       1415 + RingBuf.delete buf;
 *       TODO Revisit to understand monotypes and polymorphism. *)

(* 8.19: Pointing to the same object, normal reference equality? (Yes).
 *       Same as Arrays mentioned later. *)

(* 8.20: Point of this one is to use memoization, like matrix multiplication. *)
(* Original function, for reference. *)
fun allC (coins, vals, amt) =
  let fun change (coins, vals, 0, result)      = coins::result
        | change (coins, [], amt, result)      = result
        | change (coins, v::vals, amt, result) =
            if amt < 0 then result
            else change (coins, vals, amt,
                    change (v::coins, v::vals, amt-v, result))
   in change (coins, vals, amt, [[]]) end;

(* Number of solutions only *)
fun allC_Length (vals, amt) =
  let fun change (vals, 0, result)      = result+1
        | change ([], amt, result)      = result
        | change (v::vals, amt, result) =
            if amt < 0 then result
            else change (vals, amt,
                    change (v::vals, amt-v, result))
   in change (vals, amt, 1) end;
 
(* My attempt, after I realised
 *    (a) memoization and
 *    (b) Array and not Varray *)
fun allC_Eff (vals, amt) =
  let open Array
      val result = array (amt, NONE)

      fun change (vals, 0)      = 1
        | change ([], amt)      = 0
        | change (v::vals, amt) =
            if amt < 0 then 0 else
              let val rest = case (sub (result, amt-1)) of 
                         NONE   => change (vals, amt)
                       | SOME a => a

                  val sum  = change (v::vals, amt-v) + rest

              in  update (result, amt-1, SOME sum); sum end

  in change (vals, amt); sub(result, amt-1) end;

(*    (c) association list needed to keep track of coins AND value. *)
fun allC_Eff (vals, amt) =
  let open Array
      val result = array (amt, NONE)

      fun assoc (NONE, _) = NONE
        | assoc (SOME [], a) = NONE
        | assoc (SOME ((x,y)::xs), a) =
            if a=x then SOME y else assoc (SOME xs, a)

      fun change (vals, 0)      = 1
        | change ([], amt)      = 0
        | change (v::vals, amt) =
            if amt < 0 then 0 else
              let val sum = case (assoc (sub (result, amt-1), v::vals)) of
                      NONE   => change (vals, amt) + change (v::vals, amt-v)
                    | SOME a => a
                  val rest = case (sub (result, amt-1)) of
                      NONE   => []
                    | SOME a => a
              in  update (result, amt-1, SOME ((v::vals, sum)::rest));
                  sum end
  in change (vals, amt); sub(result, amt-1) end;

(* 8.21: See structure. *)

(* 8.22: See structure. *)

(* 8.23: *)
signature ARRAY2 =
  sig
  type 'a array
  exception Subscript and Size (* almost forgot this *)
  val array    : int * int * 'a -> 'a array
  val fromList : 'a list list -> 'a array
  val tabulate : int * int * (int * int -> 'a) -> 'a array
  val sub      : 'a array * int * int -> 'a
  val update   : 'a array * int * int * 'a -> unit
  val length   : 'a array -> int
  val length2  : 'a array * int -> int
  end;

structure Array2 :> ARRAY2 =
  struct
  local structure A = Array in
  exception Subscript and Size
  type 'a array = 'a A.array A.array
  fun array (rows, cols, x) =
    if rows < 0 orelse cols < 0 then raise Size else
      A.tabulate (rows, fn i => A.array (cols, x))

  fun fromList xs = A.fromList (map A.fromList xs)

  fun tabulate (rows, cols, f) =
    A.tabulate (rows, fn i => A.tabulate (cols, fn j => f (i,j)))

  fun sub (arr, row, col) = A.sub (A.sub (arr, row), col)

  fun update (arr, row, col, x) = A.update (A.sub (arr, row), col, x)

  fun length arr = A.length arr

  fun length2 (arr, row) = A.length (A.sub (arr, row))
  end
  end;

(* 8.24: *)
use "signatures/VARRAY.sml";
use "structures/Varray.sml";

signature VARRAY2 =
  sig
  type 'a t
  exception Subscript and Size (* almost forgot this *)
  val array      : int * int * 'a -> 'a t
  val reroot     : 'a t -> 'a t
  val sub        : 'a t * int * int -> 'a
  val justUpdate : 'a t * int * int * 'a -> 'a t
  val update     : 'a t * int * int * 'a -> 'a t
  end;

structure Varray2 :> VARRAY2 =
  struct
  local structure V = Varray in
  exception Subscript and Size

  type 'a t = 'a V.t V.t

  fun array (rows, cols, x) =
    if rows < 0 orelse cols < 0 then raise Size else
      V.array (rows, V.array (cols, x))

  fun reroot arr = V.reroot arr

  fun sub (arr, row, col) = V.sub (V.sub (arr, row), col)

  fun justUpdate (arr, row, col, x) = 
    let val sub_arr = V.justUpdate (V.sub (arr, row), col, x)
    in  V.justUpdate (arr, row, sub_arr) end

  fun update (arr, row, col, x) =
    let val sub_arr = V.update (V.sub (arr, row), col, x)
    in  V.update (arr, row, sub_arr) end
  end
  end;

(* 8.25: length, index and so on, the element is not used but there. It's needed
 * to provide a link to the Array but is a bit useless/redundant. *)

(* 8.26: "Permissive" *)
fun writeCheque w (dols, cents) =
  "$"^StringCvt.padLeft #"*" (w-4) (Int.toString dols)^
  "."^StringCvt.padLeft #"0" 2     (Int.toString cents);

(* 8.27: *)
val toUpper = String.map Char.toUpper;

(* 8.28: *)
Bool.scan Substring.getc (Substring.full "mendacious");
Bool.scan Substring.getc (Substring.full "falsetto");
Real.scan Substring.getc (Substring.full "6.626x-34");
Int.scan StringCvt.DEC Substring.getc (Substring.full "1o24");

(* 8.29: Looked at online solution to find StringCvt.splitl, which addmittedly,
 * was in the book in the firstLine example. *)
fun scanDate getc src =
  let val months            = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN",
                               "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"]
      val SOME (day, rest)  = Int.scan StringCvt.DEC getc src
      val SOME (#"-", m_y)  = Char.scan getc rest (* getc rest also works *)
      val (mon', rest)      = StringCvt.splitl Char.isUpper getc m_y
      val mon               = String.substring (mon', 0, 3) 
      val SOME (#"-", year) = getc rest           (* like so *)
      val SOME (yr, _)      = Int.scan StringCvt.DEC getc year
  in  if List.exists (secr op = mon) months then SOME (day, mon, yr) else NONE
  end handle Bind => NONE;

(* 8.30: *)
fun countStuff is =
  let
    val lines  = ref 0
    and chars  = ref 0
    and words  = ref 0
    and null   = secr op = 0 o Substring.size
    and nxtWrd = Substring.dropl Char.isSpace o
                 Substring.dropl (not o Char.isSpace)
    (* Done to avoid creating intermediate list of String.tokens *)
    fun cntWrd (num, substr) = if null substr then num
                               else cntWrd (num+1, nxtWrd substr)
  in 
    while not (TextIO.endOfStream is)
    do (let val SOME str = TextIO.inputLine is
        in  lines := !lines + 1;
            chars := !chars + size str;
            words := !words + cntWrd (0, Substring.full str)
        end);

    TextIO.closeIn is;

    (!lines, !words, !chars)
  end;

(* 8.31: For some reason, it printed error before first input.
 *       Hence, ugly bug fix. *)
fun circleArea (is, os) =
  let fun area r  = "Area: "^ Real.toString (Math.pi * r * r) ^ "\n"
      val (fst, f') = (ref true, ref true)
      val prompt  = fn () => (if !fst = !f' then print "Input radius: "
                              else f' := false; not (TextIO.endOfStream is))
  in  while (prompt())
      do     let val SOME s = TextIO.inputLine is
             in  case Real.fromString s of
                    NONE   => if !fst then fst := false
                              else print "Try again\n"
                  | SOME r => print (area r) end end;

(* 8.32: Improved after seeing String.translate in online solution.
 *       Online solution does val putTagsIO = putTags o TextIO.inputLine,
 *       which might be easier to maintain/change. *)
fun htmlTags #"<"  = "&lt;"
  | htmlTags #">"  = "&gt;"
  | htmlTags #"&"  = "&amp;"
  | htmlTags #"\"" = "&quot;"
  | htmlTags s     = String.str s;

val putTags = String.translate htmlTags;

fun firstLine s =
  let val (name,rest) = Substring.splitl (secr op <> #".") (Substring.full s)
  in "\n<P><EM>"^Substring.string name^"</EM>"^Substring.string rest end;

fun htmlCvt fileName =
  let
    val is = TextIO.openIn fileName
    and os = TextIO.openOut (fileName ^ ".html")
    fun cvt _ NONE         = ()
      | cvt _ (SOME "\n")  = cvt true (TextIO.inputLine is)
      | cvt first (SOME s) =
        (TextIO.output (os, putTags (if first then firstLine s else "<BR>"^s));
         cvt false (TextIO.inputLine is))
  in
    cvt true (SOME "\n");
    TextIO.closeIn is;
    TextIO.closeOut os
  end;

(* 8.33: It would do that if the break condition was not satisfied, namely
 *         if len + breakdist(es,after) <= !space 
 *         then blanks len
 *         else (newline();  blanks(margin-blockspace));
 *       so if the distance to the next break was too large to fit on line.
 *       
 *       The online solution gives the example
 *       prettyshow (Disj (Conj (Atom "very_rich_indeed", Atom "poor"),
 *                         Conj (Atom "rich", Atom "fine"))) for width 25.
 *       In this case, indent the next line and then put the next break on a
 *       new line and undo the indent to emphasise separate clauses. *)
use "signatures/PRETTY.sig";
use "structures/Pretty.sml";

(* 8.34: You'd need a new dataype of a boolean in Block for consitency on or
 * off. Since code would be clearer with datatype (instead of if and else
 * statements everywhere) try
 *
 *    CBlock of t list  * int * int
 *    val cblo : int * t list -> t
 *
 * and then the main one would be a
 *
 *    List.app (fn x => newline(); printing (x, !space-indent, ...)) bes
 *
 * in the printing function. Online solution suggests adding a boolean. *)

(* 8.35: Would have been able to do it had I realised
 *       (a) the marked line that progresses through the rest of the list
 *       (b) attach functions needed to construct result up the way.
 *       Tried nesting recursive calls in a last, accumulating argument like
 *       making change. *)
structure Pretty =
  struct
  (*Printing items: compound phrases, strings, and breaks*)
  datatype t = 
      Block of t list * int * int 	(*indentation, length*)
    | String of string
    | Break of int;			(*length*)

  (*Add the lengths of the expressions until the next Break; if no Break then
    include "after", to account for text following this block. *)
  fun breakdist (Block(_,_,len)::es, after) = len + breakdist(es, after)
    | breakdist (String s :: es, after) = size s + breakdist (es, after)
    | breakdist (Break _ :: es, after) = 0
    | breakdist ([], after) = after;

  (* Imperative *)
  fun pr (os, e, margin) =
   let val space = ref margin

       fun blanks n = (TextIO.output(os, StringCvt.padLeft #" " n "");  
		       space := !space - n)

       fun newline () = (TextIO.output(os,"\n");  space := margin)

       fun printing ([], _, _) = ()
	 | printing (e::es, blockspace, after) =
	  (case e of
	       Block(bes,indent,len) =>
		  printing(bes, !space-indent, breakdist(es,after))
	     | String s => (TextIO.output(os,s);   space := !space - size s)
	     | Break len => 
		 if len + breakdist(es,after) <= !space 
		 then blanks len
		 else (newline();  blanks(margin-blockspace));
	    printing (es, blockspace, after)) (* <---- REST OF LIST *)
   in  printing([e], margin, 0);  newline()  end;

  (* Functional *)
  fun toString (e, margin) =
      let fun blanks 0 = "" | blanks n = " " ^ blanks (n-1)

        fun printing ([], _, _, sp, r) = (r, sp)
          | printing (e::es, blocksp, after, sp, r) =
            let val (res, sp') = case e of 

                     Block (bes, ind, len) =>
                       printing (bes, sp-ind, breakdist (es, after), sp, "")

                   | String s   => (s, sp - size s)

                   | Break  len =>
                       if len + breakdist (es, after) <= sp then
                         (blanks len, sp-len)
                       else
                         ("\n" ^ blanks (margin-blocksp), blocksp)

            in printing (es, blocksp, after, sp', r^res) end

    in  #1(printing ([e], margin, 0, margin, "")) end

  fun length (Block(_,_,len)) = len
    | length (String s) = size s
    | length (Break len) = len;

  val str = String  and  brk = Break;

  fun blo (indent,es) =
    let fun sum([], k) = k
	  | sum(e::es, k) = sum(es, length e + k)
    in  Block(es,indent, sum(es,0))  end;
  end;

datatype prop = Atom of string
              | Neg  of prop
              | Disj of prop * prop
              | Conj of prop * prop;

local open Pretty
  in fun prettyshow (Atom a) = str a
       | prettyshow (Neg p) = 
           blo(1, [str"(~", prettyshow p, str")"])
       | prettyshow (Conj(p,q)) = 
           blo(1, [str"(", prettyshow p, str" &",  
               brk 1, prettyshow q, str")"])
       | prettyshow (Disj(p,q)) =
           blo(1, [str"(", prettyshow p, str" |",  
               brk 1, prettyshow q, str")"]);
  end;

fun implies (p, q) = Disj(Neg p, q);

val rich    = Atom "rich"
and landed  = Atom "landed"
and saintly = Atom "saintly";

val asm1 = implies (landed, rich)
and asm2 = Neg(Conj(saintly, rich))
and concl = implies (landed, Neg saintly);

val goal  = implies (Conj(asm1, asm2), concl);

(*
val p = print (
*)
val p_str = Pretty.toString (prettyshow goal, 60);
print p_str;
Pretty.pr(TextIO.stdOut, prettyshow goal, 60);

(* 8.36: You'd need a block as above, but also contructors for the placeholders 
 *       I and F (e.g. Int of int and Flt of int * int) to specify places.
 *       All this would be under one datatype.
 *
 *       Online solution:
 *       ----------------
 *       For input, write an interpreter that takes a string and a list of
 *       format specifications, reads the string and returns a list of data
 *       items. Output would be done analogously.
 *
 *       This would, as in Fortran, allow formatted I/O for certain types fixed
 *       in advance. To allow the collection of types to be extended at any
 *       time, you could use type exn in its role as an extensible datatype,
 *       declaring new constructors (as exceptions) for new format
 *       specifications whenever desired. Some means of extending the system
 *       with new read/print routines would have to be worked out also.  *)
