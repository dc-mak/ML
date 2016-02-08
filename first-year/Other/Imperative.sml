(* See http://mlton.org/ForLoops for further info. *)

(* For loops *)
fun for (count, pred, next) f =
  if not(pred count) then fn x => x
  else fn x => (for (next count, pred, next) f) (f x);

(* With index available, returns only result *)
fun fori (cond as (start, pred, next)) f =
let 
  fun fi (count, pred, next) f =
    if not(pred count) then fn x => (count, x)
    else fn x => (fi (next count, pred, next) f) (f (count, x))
in
  fn x => #2((fi cond f) x)
end;

(* Common iterators *)
infix to;
fun a to b = 
let
  val (pred, nxt) = if a < b then (op<=, op+) else (op>=, op-)
in
  (a, fn x => pred(x,b), fn x => nxt(x,1))
end;

(* Examples *)
fun loopPrnt (i,r) = print ("Loop "^Int.toString(i)^"\n");

val loop = fori (9 to 0) loopPrint;

val loop2 = (fori (0 to 9) op+) 0;
