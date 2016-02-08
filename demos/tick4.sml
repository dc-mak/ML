(* Dhruv Makwana, IA ML Demonstrator *)
(* What I'd write *)
fun nfold f 0 = (fn x => x)
  | nfold f n = nfold f (n-1) o f
             (* f o nfold f (n-1) *)

(* Probably what's expected *)
fun nfold2 f 0 = (fn x => x)
  | nfold2 f n = fn x => f ((nfold2 f (n-1)) x)
                     (* (nfold2 f (n-1)) (f x) *)

(* For understanding *)
fun nfold4 f 0 = (fn x => x)
  | nfold4 f n =
    let val f_m = nfold4 f (n-1)
    in  fn x => f (f_m x) end
    (*  fn x => f_m (f x) end *)

(* Add one to y, x times *)
fun sum x y = (nfold (fn x => x+1) x) y

(* Add x to 0, y times *)
fun product x y = (nfold (sum x) y) 0

(* Multiply 1 by x, y times *)
fun power x y = (nfold (product x) y) 1

(* Aaaand taking into account sign *)
fun sign 0 = 0
  | sign x = if x < 0 then ~1 else 1

fun abs x = x * sign x

fun safe_sum x y = (nfold (fn z => z + sign x) (abs x)) y

fun safe_product x y = (nfold (safe_sum x) (abs y)) 0 * sign y

(* Return a fraction *)
fun safe_power x y =
  let val pow = (nfold (safe_product x) (abs y)) 1
  in   if y >= 0 then (pow, 1) else (1, pow) end

(* Q2: streams *)
datatype 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

fun nth (xf, n) =
  if n < 1 then raise Subscript else
  let fun nth' (Nil, _)        = raise Empty
        | nth' (Cons(x,xf), 1) = x
        | nth' (Cons(x,xf), n) = nth (xf(), n-1)
  in  nth' (xf, n) end

fun from k = Cons(k, fn() => from (k+1))

(* Q3: What I'd do *)
fun mapq f Nil = Nil
  | mapq f (Cons(x,xf)) = Cons(f x, fn() => mapq f (xf()))

val squares = mapq (fn x => x*x) (from 1)

(* Maybe what's expected? *)
fun square_from k = Cons(k*k, fn() => square_from (k+1))

(* Q4 *)
fun map2 f Nil _ = Nil
  | map2 f _ Nil = Nil
  | map2 f (Cons(x, xf)) (Cons(y,yf)) =
      Cons(f x y, fn() => map2 f (xf()) (yf()))
