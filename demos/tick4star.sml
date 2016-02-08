(* Dhruv Makwana, IA ML Demonstrator *)
use "tick4.sml";

fun i x        = x
fun k x y      = x
fun s x y z    = x z (y z)
fun secl f x y = f (x,y) (* or curry *)
fun secr f y x = f (x,y)

fun dropq (xf,  0)         = xf
  | dropq (Nil, _)         = Nil
  | dropq (Cons(x,xf) , n) = dropq(xf(), n-1)

val tailq = secr dropq 1

fun toList (_,   0)         = []
  | toList (Nil, _)         = []
  | toList (Cons(x, xf), n) = x::toList(xf(), n-1)

(* Can also use if-else-if-else. BUT, the problem with doing that is typing
 * something like "if x > y then ... else if y > x then ... else ...", because
 * x < y and y > x are the same predicate. *)
fun merge (Nil, yf) = yf
  | merge (xf, Nil) = xf
  | merge (xq as Cons(x, xf), yq as Cons(y, yf)) =
    let val (min, xf', yf') =

          case Int.compare (x,y) of 
               LESS    => (x, xf, fn() => yq)
             | GREATER => (y, fn() => xq, yf)
             | EQUAL   => (x, xf, yf)

    in  Cons(min, fn() => merge(xf'(), yf'())) end

(*
   if x < y then Cons(x, fn() => merge (xf(), yq)) else
   if y < x then Cons(y, fn() => merge (xq, yf())) else
                 Cons(x, fn() => merge (xf(), yf()))
*)

(* Q2. Assume neither xq and yq contain no duplicates [viz. merge(xq, yf())] *)
fun times23 () = 
  Cons(1, fn() => merge (mapq (secl op* 2) (times23()),
                         mapq (secl op* 3) (times23()))) 

val pows23 = times23()

(* Q3. Note the dropq to remove 1,1 and 5,5 and other duplicates *)
fun times235 () =
  Cons(1, fn() => merge(mapq (secl op* 5) (times235()), tailq (times23())))

val pows235 = times235 ()

(* Doesn't terminate for multiply recursive streams such as times23[5].
 * I imagine because streams in a tuple must/end up being evaluated immediately. *)
fun merge_bad (Nil, yf) = yf
  | merge_bad (xf, Nil) = xf
  | merge_bad (xq as Cons(x, xf), yq as Cons(y, yf)) =
      let val (min, xf', yf') =
            if x < y then (x, xf(), yq) else
            if y > x then (y, xq, yf()) else
            (* x = y *)   (x, xf(), yf())
      in  Cons(min, fn() => merge_bad(xf', yf')) end

(* "Better" version. However, what's wrong with this one..? *)
fun merge_better (Nil, yf) = yf
  | merge_better (xf, Nil) = xf
  | merge_better (xq as Cons(x, xf), yq as Cons(y, yf)) =
      let val (min, xf', yf') =
              if x < y then (x, xf, fn() => yq) else
              if y > x then (y, fn() => xq, yf) else
              (* x = y *)   (x, xf, yf)
      in  Cons(min, fn() => merge_better(xf'(), yf'())) end
