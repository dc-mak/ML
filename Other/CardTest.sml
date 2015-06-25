(* 1995, Paper 1, Question 6 *)
(* Dhruv Makwana *)

use "Useful.sml";

(* Unsigned *)
fun toBitList n =
  let
    fun half n =  if (n mod 2 = 0) then zero else one
    fun tC 0 [] = [zero]
      | tC 0 rs  = rs
      | tC n rs =
        tC (n div 2) ((half n)::rs) 
  in
    rev (tC n [])
  end;

val toCard = Cardinal o toBitList;

fun toInt (Cardinal xs) = 
  #2(foldl (fn (x,(n,t)) => (n*2, (case x of zero => 0 | one => 1)*n + t)) (1,0) xs);

infix 0 ++;
fun a ++ b = toInt (add (toCard a) (toCard b));

infix 1 **;
fun a ** b = toInt (mult (toCard a) (toCard b));

(* Two's complement *)
fun toSgnInt (Cardinal xs) = 
  let
    fun tSI [one] n t = t - n
      | tSI [] n t = t
      | tSI (x::xs) n t = 
      case x of zero => tSI xs (2*n) t
         | one =>       tSI xs (2*n) (t+n)
  in
    tSI xs 1 0
  end;

(* Only for negative numbers *)
fun extend n =
  let
    val bitList = toBitList (~n)
    fun ext [] = raise Empty
      | ext [one] = [one, zero]
      | ext [zero] = [zero]
      | ext (x::xs) = x::ext xs
  in
    ext bitList
  end;

fun toSgnBits 0 = [zero]
  | toSgnBits n =
    if n < 0 then
      (negate o extend) n
    else
      (toBitList n)@[zero];

val toSgnCard = Cardinal o toSgnBits;

infix 0 --;
fun a -- b = toSgnInt (subtr (toSgnCard a) (toSgnCard b));

(* Test stuff *)
(* Basics *)

(* To test toSgnInt *)
fun cons n x = n::x;
fun sBits () = Cons([], fn () => inter (seqMap (cons zero) (sBits())) 
                                     (seqMap (cons one) (sBits())));
fun bitString x = case x of zero => "zero " | one => "one ";
fun bitListString [] = "\n"
  | bitListString (x::xs) = (bitString x)^(bitListString xs);

(*
val bitList = tl (get 32 (s01 ()));
val dummy = map (print o bitListString) bitList;
val dummy = map (print o (fn x => Int.toString(x)^"\n")) testSgnInt;
*)

(* To test toSgnBits *)
fun eqCheck [] [] = true
  | eqCheck [x] [y] = x = y
  | eqCheck (x::xs) (y::ys) = (x = y) andalso (eqCheck xs ys)
  | eqCheck _ _ = false;

(*
val intList = List.tabulate(10, 
                fn i => if i mod 2 = 1 then i div 2 else ~i div 2);
val testSgnBits = map toSgnBits intList;
val testSgnInt = map (toSgnInt o Cardinal) testSgnBits;
eqCheck testSgnInt intList;
*)

(*
To test subtract
val result = map (fn (x,y) => (x, y, x--y)) 
                 [(7,4),(4,7),(~7,4),(4,~7),
                 (7,~4),(~4,7),(~7,~4),(~4,~7)];

fun show x y z = print (Int.toString(x)^
                        " - "^Int.toString(y)^
                        " != "^Int.toString(z)^
                        " not "^Int.toString(x-y)^
                        "\n");

fun check xs = map (fn (x,y,z) => 
                      if (x - y) <> z then (show x y z)
                      else ())
               xs;
check result;
*)

(* Last big check *)
val ys = List.tabulate(3, fn i => i - 3 div 2);
val testList = List.concat(map (fn x => map (fn y => (x,y)) ys) ys);
fun check f g xs =
  foldl (fn ((x,y),e) => (f(x,y)) = (g(x,y)) andalso e) true xs;
(*
val addTest = check op++ op+ testList;
val mulTest = check op** op* testList;
val subTest = check op-- op- testList;
*)
