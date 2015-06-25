(* Prelim CS2 Question 2
*  --------------------
*  Analyse the time complexity of:
fun rvs [] = ([])
  | rvs (a::b) = app (rvs b) [a]

and app [] b = b
  | app a b = app (rvs (tl (rvs a))) (hd(rvs a)::b);

R(0)         =   1
R(n)         =   A(n-1, 1) + R(n-1)
R(n-1)       =   A(n-2, 1) + R(n-2)

A(0, t)      =   1
A(s, t)      =   A(s-1, t+1) + R(n-1) + 2R(n)
A(n-1, 1)    =   A(n-2, 2) + R(n-2) + 2R(n-1)

R(n)         =   A(n-2, 2) + R(n-2) + 3R(n-1)
             =   R(n-1) + 3R(n-1)
             =   4R(n-1)
* ----------------------- *)

(*2007 Paper 1 Question 6
* ----------------------
* Nothing special
* ---------------------
datatype 'a meal = Snack of 'a | Lunch of 'a meal * 'a meal
                 | Feast of 'a meal * 'a meal * 'a meal;

fun snack m = 
let val l = ref []
    fun munch (Snack x) = (l := x:: !l)
      | munch (Lunch (m1, m2)) = (munch m1; munch m2)
      | munch (Feast (m1, m2, m3)) = 
            (munch m1; munch m2; munch m3)
in munch m; !l end;

fun snacker m =
let 
    fun mun(Snack x) = [x]
      | mun(Lunch (m1, m2)) = mun(m2)@mun(m1)
      | mun(Feast (m1, m2, m3)) =
            mun(m3)@mun(m2)@mun(m1)
in
    mun m
end;

fun gluttony [] m2 = []
  | gluttony m1 m2 = 
  let val replace = case hd(m1) of 
                         (Snack x) => m2
                       | (Lunch (x1, x2)) => hd(m1)
                       | (Feast (x1, x2, x3)) => hd(m1)
  in
      replace::(gluttony (tl(m1)) m2)
  end;

fun glut k [] m2 = []
  | glut 0 m1 m2 = m1
  | glut k m1 m2 =
  let val (s, t) = case (k, hd(m1)) of
                   (1, Snack x) => (0, m2)
                 | (1, Lunch (x1, x2)) => (1, hd(m1))
                 | (1, Feast (x1, x2, x3)) => (1, hd(m1))
                 | (_, Snack x) => (k-1, hd(m1))
                 | (_, Lunch (x1, x2)) => (k, hd(m1))
                 | (_, Feast (x1, x2, x3)) => (k, hd(m1))
  in
      t::(glut s (tl(m1)) m2)
  end;
(* --------------------- *)

(* 2008 Paper 1 Question 5
* -----------------------
* Lazy lists and lazy binary trees
* ---------------------- 
datatype 'a seq = Cons of 'a * (unit -> 'a seq) | Nil;
datatype 'a ltree = Lf | Br of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);

fun head (Cons(x, _)) = x;

fun tail (Cons(_, xf)) = xf();

fun get (_, Nil) = []
  | get (0, xf) = []
  | get (n, xf) = head(xf)::(get(n-1, tail(xf)));

fun interleave (Nil, ys) = ys
  | interleave (Cons(x, xf), ys) =
  Cons(x, fn() => interleave(ys, (xf())));

fun makeints n = Br(n, fn() => makeints(n-1), fn() => makeints(n+1));

fun lztreelist Lf = Nil
  | lztreelist (Br(a, tlf, trf)) =
  Cons(a, fn () => interleave(lztreelist(tlf()), lztreelist(trf())));

fun show [] = print "|-|"
   | show x =
   let
     fun spread([], p, n) = print "\n|-|\n"
       | spread(x, 0, n) = (print "\n"; spread(x, 2*n, 2*n))
       | spread(x::xs:int list, p, n) = (print(Int.toString(x)^" "); spread(xs, p-1, n))
    in
     spread(x, 1, 1)
    end;

val test = makeints 0;
val run = lztreelist test;
val runlist = get(63, run);
show runlist;
(* -------------------- *)

(*2007 Paper 1 Question 5
-------------------------
Binary Trees
-------------------------
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

exception Blair;

fun tony p Lf = true
  | tony p (Br(x, t1, t2)) =
    if not(p x) then raise Blair
    else tony p t1 handle Blair => tony p t2;

fun gordon p x = tony p x handle Blair => false;

fun gordon2 p Lf = true
  | gordon2 p (Br(x, Lf, t2)) = p x
  | gordon2 p (Br(a, Br(x, t11, t12), t2)) =
    if not(p a) then false 
    else if not(p x) then gordon2 p t2
         else gordon2 p t11;
(* ---------------------- *) 

(*1993 Paper 1 Question 5
-------------------------
Lazy Lists
-------------------------
datatype 'a seq = Nil | Cons of unit -> 'a*'a seq;

fun infinite n = Cons (fn() => (n, infinite(n+1)));

exception Empty;
fun hd (Cons(x)) = #1(x())
  | hd Nil = raise Empty;

fun tl (Cons(x)) = #2(x())
  | tl Nil = raise Empty;

fun first (Cons(x)) = x()
  | first Nil = raise Empty;

fun next(_, Cons(x)) = x()
  | next (_, Nil) = raise Empty;

exception Butlast;
fun butlast Nil = raise Butlast
  | butlast list = first(list);
(* ----------------------- *)
