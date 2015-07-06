(* 5.1: *)
fn x     => x * x : real;
fn (x,y) => x::y;
fn []    => true | (_::_) => false;

(* 5.2: *)
val area      = fn r     => Math.pi * r * r;
val title     = fn name  => "The Duke of "^name;
val lengthvec = fn (x,y) => Math.sqrt (x*x + y*y);

(* 5.3: First one, function that adds i to the argument.
*       Second, function that checks if argument is less than a.
*       Third, function that creates a two-tuple with x in front.
*       Fourth, function that checks if argument is equal to x. *)

(* 5.4: If h is also curried, then no, otherwise yes.
 *
 * Online solution: if (f x) is going to be applied to an argument y more than
 * once, the second one is more efficent becasuse it does not re-evaluate
 * f = fn x => h (g x) every time by binding it to the identifier f.
 * With the first, calling (f x) returns a function over y but in the second, it
 * evaluates h (g x). So, if h (g x) raises an exception or doesn't terminate
 * then the second will respond immediately whilst the first will wait for the
 * application of y.*)

(* 5.5: (string * (real -> real)) Dict.t * string -> (real -> real). *)

(* 5.6: Online solution uses let...in to reduce copies of compare passed. *)

fun merge compare ([], ys)       = ys : real list
  | merge compare (xs, [])       = xs
  | merge compare (x::xs, y::ys) =
      if compare (x,y) then x::merge compare (xs, y::ys)
                       else y::merge compare (x::xs, ys);

fun tmergesort compare []  = []
  | tmergesort compare [x] = []
  | tmergesort compare xs  =
    let val k = length xs div 2
    in  merge compare (tmergesort compare (List.take (xs,k)),
                       tmergesort compare (List.drop (xs,k))) end;

(* 5.7: Assume integer return. Corrected with online solutions to avoid
 * infinite looping on m=0. *)
exception Minimum;
fun minimum f 0 = raise Minimum
  | minimum f m =
  let fun mini (i,z) =
        if i=m then z else
        mini (i+1, Int.min(z, f i))
  in  mini (1, f 0) end;

(* g can be curried. *)
fun min2 g m n =
    minimum (fn i => minimum (fn j => g(i,j)) n) m;

(* 5.8: Well you can define them with so yeah, they're
 *   val secl = fn x => fn f => fn y => f (x,y)
 *   val secr = fn f => fn y => fn x => f (x,y)
 * basically turning infix operators into a curried function. *)

(* 5.9: The first one appends ["Richard"] on the end of any list.
 *
 * The second is not good form, since List.take is not infix, but returns the
 * function of type int -> string list for the first n elements of that list.
 *
 * Function that takes the first 3 elements of any given list.
 *
 * Function that interleaves the list second to any given one. *)

(* 5.10: fun S x y z  = x z (y z) and fun K x y = x
 *           S K K 17 = (fn z => K z (K z)) 17
 *                    = (fn z => z) 17
 *                    = 17  *)

(* 5.11 Constants can be replaced with K.
 *  Infix operators can be made curried with secl and secr.
 *  Composition allows the value x to be "piped" through.
 *  Composing with the identity function allows x to be used with any infix
 *  functions with secl and secr, the result of which itself can be used in
 * computation.
 *
 * Online solution: since there is only one occurence, we can inductively
 * define the function/abstractions. Let '#' represent any arbitrary infix
 * operators in E.
 *          [x]x        = I
 *          [x] (M # N) = (secr # N) o ([x]M) if x is in M
 *          [x] (M # N) = (secl M #) o ([x]M) if x is in N. *)

(* 5.12: map f (map g xs) = map (f o g) xs *)

(* 5.13: *)
infixr 5 andf;
fun f andf g = fn x => (f x) andalso (g x);

(* 5.14: Combinators *)
fun k x y      = x;
fun s x y z    = x z (y z);

(* Purely just for readability and convenience in currying two-tuple functions. *)
fun secpre f x y = f (x,y);
fun secinf x f y = f (x,y); (* secl *)
fun secpos f y x = f (x,y); (* secr *)

infix mem;
fun x mem xs = List.exists (secpre op= x) xs;

fun newmem (x,xs) = if x mem xs then xs else x::xs;

fun union (xs, ys) = foldl newmem ys xs;

(* 5.15: It tried to avoid fn Pat => E but and sacrified storage/reducing
 * copying for readability. *)
fun transp ([]::_) = []
  | transp rows    = map hd rows::transp (map tl rows);

(* Could have done composition -
 * val dotprod = foldl op+ 0 o ListPair.mapEq op* *)
val dotprod = ListPair.foldlEq (fn (x,y,e) => x*y+e) 0;
(* Online solution
 * fun dotprod pairs = fold op+ 0.0 (ListPair.mapEq op* pairs) *)

fun matprod (rowsA, rowsB) =
  let fun rowprod cols row = map (secpre dotprod row) cols
  in  map (rowprod (transp rowsB)) rowsA end;
(* Online solution:
 * let val colsB = transp rowsB
 * in  map (fn row => map (fn cols => dotprod (row,col)) colsB) rowsA *)

(* 5.16: Not lazy enough, traverses whole list. *)
fun exists' pred = foldl (fn (x,e) => pred x orelse e) false;

(* Testing the fastest for cartestian product. *)
fun fcprod xs ys = foldr (fn(x,e) => (foldr (fn(y,l)=>(x,y)::l) e ys)) [] xs;
fun lcprod xs ys = List.concat (map (fn x => map (fn y => (x,y)) ys) xs);
local val (a,m) = (1678.0, 2147483647.0) in
      fun nextrand seed =
      let val t = a * seed
      in t - m * real (floor (t/m)) end
end;
fun randlist (n, seed, tail) =
      if n = 0 then (seed, tail)
      else randlist (n-1, nextrand seed, seed::tail);

fun makexs() = #2(randlist (7200, 1.0, []));

fun time cartprod xs  =
 let
   val cPU_time   = Timer.startCPUTimer()
   and real_time  = Timer.startRealTimer()
   val cartesian  = cartprod xs xs
 in
   (Timer.checkCPUTimer cPU_time, Timer.checkRealTimer real_time)
  end;
(* > time fcprod xs_eval;
 * val it = ({sys = 1.459, usr = 37.881}, 28.890):
 *    {sys: Time.time, usr: Time.time} * Time.time
 * > time lcprod xs_eval;
 * val it = ({sys = 1.872, usr = 426.993}, 320.633):
 *    {sys: Time.time, usr: Time.time} * Time.time   *)

(* 5.17: Corrected to remove redundant set mem check. Online solution does it
 * with nested foldls, which I reduced copying but makes it less readable. *)
fun set_sub (xs, ys) =
  let
    fun pairup (x,pairs) = foldr (fn(y,l) => (x,y)::l) pairs ys
    val cartprod         = foldr pairup [] xs
    fun filter ((x,y),e) = if y>x then newmem (y-x,e) else e
  in
    foldl filter [] cartprod
  end;
(* fun posDiffs (xs,ys) =
 *     foldr (fn (x,e) => (
 *       foldr (fn (y,l) => if y<x then y-x::l else l) e ys))
 *     [] xs;       *)

(* 5.18: *)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun prefold f e Lf              = []
  | prefold f e (Br(x, tl, tr)) = f(x, prefold f (prefold f e tr) tl);

(* 5.19: *)
fun repeat f n x =
    if n > 0 then repeat f (n-1) (f x)
             else x;

fun fib n =
  let fun fibaux (n, k) = (k, n+k)
  in  repeat fibaux n (0,1) end;

(* 5.20: For creating repeat n in log n time, made along the same lines as the
 * exponentiation function. *)

(* 5.21: The type of treefold: ('a * 'b * 'b -> 'b) -> 'b -> 'a tree -> 'b
* The type of F
*   'a * 'b * 'c -> 'd -> 'e
* where 'e = 'a list because of :: so
*   'a * 'b * 'c -> 'd -> 'a list
* f1 and f2 are functions, where f2's return is of type f1's input
*   'a * ('f -> 'g) * ('d -> 'f) -> 'd -> 'a list
* and 'g = 'a list since f2 must return and 'a list so
*   'a * ('f -> 'a list) * ('d -> 'f) -> 'd -> 'a list
* but we know from the type of treefold that 'b = 'c so,
* 'd = 'f = 'a list so
*   'a * ('a list -> 'a list) * ('a list -> 'a list) -> 'a list -> 'a list.
*
* So, given the first two arguments treefold F I's type is
*   'a tree -> ('a list -> 'a list).
*
* I think, it prepends the preorder of a given tree to a given list. *)

(* 5.22: Counting. *)
datatype term = Var of string | Fun of string * term list;

fun funs (Var a)         = 0
  | funs (Fun (f, args)) = foldl op+ 1 (map funs args);

fun accumFuns (Var a, bs)       = bs
  | accumFuns (Fun (f,args), t) = foldr accumFuns (t+1) args;

(* Simple counting funs. Online solution uses mutual recursion on lists, which
 * makes the note about treating terms term lists as mutually recursive
 * datatypes make sense. *)
fun simFuns (Var a)         = 0
  | simFuns (Fun (f, args)) =
    let fun sumFuns []      = 0
          | sumFuns (x::xs) = simFuns x + sumFuns xs
    in  1 + sumFuns args end;

(* 5.23: *)
fun setVars (Var a, bs)        = newmem (a, bs)
  | setVars (Fun(_, args), bs) = foldl setVars bs args;

(* 5.24: Execution show that badfrom will never terminate and because of strict
 * evaluation, badfrom is initiated/called.
 *       take(badfrom 30, 2)
 *       take(cons (30, badfrom 31), 2)
 *       take(cons (30, ....), 2). *)

(* 5.25: *)
datatype 'a seq = Nil | Cons of unit -> 'a * 'a seq;

fun from k = Cons(fn () => (k, from (k+1)));

(* Online solution uses let...in instead. *)
fun take (xq, 0)      = []
  | take (Nil, n)     = raise Subscript  (* I'd raise empty *)
  | take (Cons yq, n) =
    if n < 0 then raise Subscript
    else case (yq()) of (x,xq) => x::take (xq, n-1);

(* 5.26: *)
datatype 'a seqnode = Nil
                    | Cons of 'a * 'a seq
and      'a seq     = Seq of unit -> 'a seqnode;

fun from k = Seq(fn () => Cons(k, from (k+1)));

(* fun force (Seq xf) = xf() *)
fun take (xq, 0)     = []
  | take (Seq yq, n) =
    if n < 0 then raise Subscript
    else case (yq()) of
              Nil          => raise Subscript
            | Cons (x, xf) => x::take (xf, n-1);

(* 5.27: *)
datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);

fun null Nil = true | null (Cons(_, _)) = false;

fun drop (xf, 0)            = xf
  | drop (Nil, n)           = raise Subscript
  | drop (Cons(x, xf), n) = drop (xf(), n-1);

fun toList Nil          = []
  | toList (Cons(x,xf)) = x::toList (xf());

(* 5.28:
 * add(from 5, squares (from 9))
 *
 *  add(Cons(5, fn () => from 6), squares (Cons 9, fn () => from 9))
 *
 *  add(Cons(5, fn () => from 6),
 *      Cons(81, fn () => squares (Cons (10, fn () => from 11))))
 *
 *  Cons (86, fn () => add (Cons(6, fn () => from 7),
 *              Cons(100, fn () => squares (Cons(11, fn () => from 12))))) *)

(* 5.29: *)
infixr app;
fun Nil app yf = yf
  | (Cons(x,xf)) app yf = Cons (x, fn () => (xf() app yf));

fun duplicate (xf, 0) = Nil
  | duplicate (Nil, k) = Nil
  | duplicate (Cons(x,xf), k) =
    Cons (x, fn () =>
      duplicate (Cons(x, fn () => Nil), k-1) app duplicate (xf(), k));

(* Online solution. *)
fun repelt k Nil          = Nil
  | repelt k (Cons(x,xf)) =
      let fun rp 0 = repelt k (xf())
        | rp k = Cons(x, fn() => rp (k-1))
      in  rp k  end;

(* 5.30: Could be done with let...in, or case as done in the online solutions. *)
fun addnext Nil     = Nil
  | addnext (Cons(x,xf)) = addprev (x, xf())
and addprev (x, Nil) = Nil
  | addprev (x, Cons(y,yf)) = Cons(x+y, fn () => addnext (yf()));

(* 5.31: If it's infinite, then you can't test a predicate over all of them. *)
fun takewhile pred Nil = Nil
  | takewhile pred (Cons(x,xf)) =
    if pred x then Cons(x, fn () => takewhile pred (xf()))
    else takewhile pred (xf());

fun dropwhile pred Nil = Nil
  | dropwhile pred (Cons(x,xf)) =
    if pred x then dropwhile pred (xf()) else Cons(x, xf);

fun exists pred Nil          = false
  | exists pred (Cons(x,xf)) = pred x orelse exists pred (xf());

(* 5.32: This 2/3/4 liner took a while. Two insights -
 * 1) e^x = 1 + x(1 + x/2 * (1 + x/3 * (1 + x/4 * (1 + ...))))
 * 2) Instead of summing over the terms generated by a sequence, we map the
 *    thing to be added accross each term.
 *
 * Alternatively, secl 1.0 op+ o secl (x / real n) op* for the map function. *)
fun tlq (Cons(x,xf)) = xf() | tlq Nil = raise Empty;

fun mapq f Nil          = Nil
  | mapq f (Cons(x,xf)) = Cons(f x, fn () => mapq f (xf()))

fun e_to x =
  let fun apprx n = Cons(1.0, fn () =>
                         mapq (fn y => 1.0 + y*x/real n) (apprx (n+1)))
  in  apprx 1 end;

(* 5.33: For testing relative difference. *)
fun within eps (Cons(x,xf)) =
  let val (xf' as Cons(y,yf)) = xf()
  in  if Real.abs(x/y-1.0) < eps then y else within eps xf' end;
