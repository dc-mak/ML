(* 2008 Paper 1 Question 5
* -----------------------
* Lazy lists and lazy binary trees
* ----------------------- *)
datatype 'a seq = Nil
                | Cons of 'a * (unit -> 'a seq) ;

datatype 'a treeq = Lf
                  | Br of 'a * (unit -> 'a treeq) * (unit -> 'a treeq);

fun head (Cons(x, _)) = x;

fun tail (Cons(_, xf)) = xf();

fun get (_, Nil) = []
  | get (0, xf) = []
  | get (n, xf) = head(xf)::(get(n-1, tail(xf)));

fun interleave (Nil, ys) = ys
  | interleave (Cons(x, xf), ys) =
    Cons(x, fn() => interleave(ys, (xf())));

fun makeints n = Br(n, fn() => makeints(n-1), fn() => makeints(n+1));

fun treeq_toSeq Lf = Nil
  | treeq_toSeq (Br(a, tlf, trf)) =
  Cons(a, fn () => interleave(treeq_toSeq(tlf()), treeq_toSeq(trf())));

fun show [] = print "|-|"
   | show x = let

     fun spread([], p, n)    =  print "\n|-|\n"
       | spread(x, 0, n)     = (print "\n"; spread(x, 2*n, 2*n))
       | spread(x::xs, p, n) = (print(Int.toString(x)^" ");
                                spread(xs, p-1, n))

    in spread(x, 1, 1) end;

val test = makeints 0;
val run = treeq_toSeq test;
val runlist = get(63, run);
show runlist;
