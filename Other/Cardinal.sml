(*1995 Paper 1 Question 6 *)
(* Dhruv Makwana *)
(* http://en.wikipedia.org/wiki/Two%27s_complement
*   has some useful tricks... *)

datatype bit = one | zero;
datatype cardinal = Cardinal of bit list;
exception Result_would_be_negative;
exception Empty;

(* Boolean functions *)
fun ! zero = one
  | ! one = zero;

infix 1 V;
fun zero V zero = zero | _ V _ = one;

infix 2 &;
fun one & one = one | _ & _ = zero;

infix 1 X;
fun a X b = ! a & b V a & ! b;

(* So that this is 1 line instead of 8 *)
fun addlogic c a b = (c X a X b, a & b V c & (a X b));

(* Carried ones on a stack *)
fun push one cs = one::cs
  | push _   cs = cs;

fun pop [] = (zero, [])
  | pop (x::xs) = (x, xs);

fun bitListAdd xs ys =
  let
    (* All remaining values are more significant *)
    fun adder [] [] cs r = rev (cs@r) (* cs only contains ones *)
      | adder [] ys [] r = rev ((rev ys)@r)
      | adder xs [] [] r = rev ((rev xs)@r)
      (* Add on any carried values in the same manner *)
      | adder [] ys cs r = adder ys cs [] r
      | adder xs [] cs r = adder xs cs [] r
      | adder (x::xs) (y::ys) cs r =
      let
        (* carry, carry list *)
        val (c, cs) = pop cs
        (* sum, carry *)
        val (s, c)  = addlogic c x y
      in
        adder xs ys (push c cs) (s::r)
      end
  in
    adder xs ys [] []
  end;

(* Only positives: And check for empty arguments *)
(* Will eventually get round to checking and dealing with
* negatives for this function. Should just be appropriate
* signs mean relevant call to subtr *)

fun add (Cardinal a) (Cardinal b) =
  let
    fun check [] b = raise Empty
      | check a [] = raise Empty
      | check a b = bitListAdd a b
  in 
    Cardinal (check a b)
  end;

(* Bit shifting *)
fun shiftL 0 ys = ys
  | shiftL n ys = shiftL (n-1) (zero::ys);

(*List of bit lists containing xs * 2^(0->n) *)
fun multiplier xs ys =
  let 
    fun m [] ys n r = r
      | m (x::xs) ys n r =
        m xs ys (n+1) (((case x of zero => [zero] | one => shiftL n ys))::r)
  in
    m xs ys 0 []
  end;

(* So that we can add all up after checking for empties *)
fun mult (Cardinal a) (Cardinal b) =
  let 
    fun check [] b = raise Empty
      | check a [] = raise Empty
      | check a b =
        foldl (fn (x,e) => add (Cardinal x) e) (Cardinal [zero]) (multiplier a b)
  in 
    check a b
  end;

(* And now attempting subtraction using 2's complement *)
fun sign xs = List.last xs;
  
(* Maintain sign/value and increase length *)
fun lengthen xs n =
  let 
    val lst = sign xs
  in
    xs@List.tabulate(n, fn i => lst)
  end;

fun negate [zero] = [zero]
  | negate [one] = [one, zero]
  | negate xs = bitListAdd (map ! xs) [one];

val negCard = (fn (Cardinal x) => (Cardinal o negate) x);

fun equalise a b =
  let
    val (lenA, lenB) = (List.length a, List.length b)
    val diff = Int.max (lenA,lenB) - Int.min (lenA,lenB)
  in
    if lenA < lenB then (lengthen a diff, b, lenB)
    else if lenB < lenA then (a, lengthen b diff, lenA)
    else (a,b,lenA)
  end;

(* equality testing *)
infix 0 ==;
fun zero == zero = true | one == one = true | _ == _ = false;

fun cut (Cardinal xs) = Cardinal (List.take(xs, List.length(xs)-1));

(* sign will raise Empty for ys and add for xs *)
fun subtr a (Cardinal [zero]) = a
  | subtr (a as Cardinal xs) (b as Cardinal ys) = 
  let
    val (sgnA, sgnB) = (sign xs, sign ys)
    val (b2 as Cardinal y2s) = negCard b
    val (x,y,l) = equalise xs y2s
    val (r as Cardinal zs) = add (Cardinal x) (Cardinal y)
    val sz = List.length(zs)
  in
    if not(sgnA == sgnB) then (cut r) else
      if sz = l then (Cardinal (zs@[zero])) else r
  end;

use "CardTest.sml";
