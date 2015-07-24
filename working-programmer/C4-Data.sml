(* 4.1: *)
datatype person = King
                | Peer of string * string * int
                | Knight of string 
                | Esquire of string * string
                | Peasant of string;

fun rank King        = 5
  | rank (Peer _)    = 4
  | rank (Knight _)  = 3
  | rank (Esquire _) = 2
  | rank (Peasant _) = 1;

fun superior (a, b) = rank a > rank b;

(* 4.2: *)
fun title King                  = "His Majest the King"
  | title (Peer (deg, terr, _)) = "The "^ deg ^" of "^ terr
  | title (Knight name)         = "Sir " ^ name
  | title (Esquire (name, vill)) = name ^ ", Esq. of " ^ vill
  | title (Peasant name)        = name;

(* 4.3: Made this easy for myself.. *)
datatype shape = Line of real
               | Triangle of real * real * real
               | Rectangle of real * real 
               | Circle of real;

fun area (Line _)               = 0.0
  | area (Triangle (a, b, c))   =     (* Had forgotten this formula *)
    let val s = (a + b + c)/2.0 in Math.sqrt (s*(s-a)*(s-b)*(s-c)) end
  | area (Rectangle (b, h))     = b * h
  | area (Circle r)             = Math.pi * r * r;

(* 4.4: *)
datatype county = Scotland | India | Spain | Portugal | France | Japan;

fun capital Scotland = "Edinburgh"
  | capital India    = "New Delhi"
  | capital Spain    = "Madrid"
  | capital Portugal = "Lisbon"
  | capital France   = "Paris"
  | capital Japan    = "Tokyo";

(* 4.5: *)
fun conj (true, true) = true
  | conj _            = false;

fun disj (false, false) = false
  | disj _              = true;

(* 4.6: King    ((unit, 'a)sum, 'b)sum
 *      Peer    (('a, string * string * int) sum, 'b) sum
 *      Knight  ('a, (string, 'b)sum)sum
 *      Peasant ('a, ('a, string)sum)sum
 * Correction: exactly the same types as before. *)

 (* 4.7: Online solutions - if datatype isn't there, we can use 
 *              ([x], []) for In1
 *              ([], [y]) for In2.  *)

(* 4.8: Online solutions - changing 'Peasant _' to 'Peasant_'. *)

(* 4.9: *)
fun title' per =
    case per of 
         King                  => "His Majest the King"
      | (Peer (deg, terr, _))  => "The "^ deg ^" of "^ terr
      | (Knight name)          => "Sir " ^ name
      | (Esquire (name, vill)) => name ^ ", Esq. of " ^ vill
      | (Peasant name)         => name;

(* 4.10: Lots of functions. *)

(* 4.11: Yes, because the they can be dynamically extended. *)

(* 4.12: See Labyrinth.sml for iterative deepening. *)

(* 4.13: Linear in number of nodes. *)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun compsame (x, 0) = Lf
  | compsame (x, n) = Br (x, compsame (x, n-1), compsame (x, n-1));

(* 4.14: Wrong. Based on online solution (which uses exceptions). *)
fun balanced Lf              = 0
  | balanced (Br(x, xl, xr)) =
   case (balanced xl, balanced xr) of
            (~1, _)     => ~1
          | (_, ~1)     => ~1
          | (l,  r)     => if abs(l-r) <= 1 then l+r+1 else ~1;
(* 4.15: *)
fun reflection (Lf, Lf)                    = true
  | reflection (Br(x,xl,xr), Br(y,yl, yr)) =
      x = y andalso reflection (xl, yr) andalso reflection (xr, yl)
  | reflection _                           = false;

(* 4.16: *)
datatype 'a nlist = Nl | Cs of 'a * 'a nlist;
(* Online solution: 
 * infixr ::;
 * datatype 'a list = nil | :: of 'a * 'a list; *)

(* 4.17: *)
datatype ('a, 'b) ltree = Lfl of 'b
                        | Brl of 'a * ('a, 'b) ltree * ('a, 'b) ltree;

(* 4.18: *)
datatype 'a btree = Lfb | Brb of 'a * 'a btree list;

(* 4.19: @ associates to the right (infixr @).
 *
 * inorder(Br("wood", Lf, Br("of", Br("Birnam", Lf, Lf), Lf) @ ["The"] @ inorder Lf
 * inorder(Br("wood", Lf, Br("of", Br("Birnam", Lf, Lf), Lf) @ ["The"]
 * inorder(Br("wood", Lf, Br("of", Br("Birnam", Lf, Lf), Lf) @ ["The"]
 * (inorder Lf @ ["wood"] @ inorder Br("of", Br("Birnam", Lf, Lf), Lf) @ ["The"]
 * (inorder Lf @ (["wood"] @ inorder Br("of", Br("Birnam", Lf, Lf), Lf)) @ ["The"]
 * (inorder Lf @ (["wood"] @ (inorder Br("Birnam", Lf, Lf) @ (["of"] @ inorder Lf)))) @ ["The"]
 * (inorder Lf @ (["wood"] @ (inorder Br("Birnam", Lf, Lf) @ ["of"]))) @ ["The"]
 * (inorder Lf @ (["wood"] @ ((inorder Lf @ ["Birnam"] @ inorder Lf) @ ["of"]))) @ ["The"]
 * (inorder Lf @ (["wood"] @ ((inorder Lf @ ["Birnam"] @ []) @ ["of"]))) @ ["The"]
 * (inorder Lf @ (["wood"] @ ((inorder Lf @ ["Birnam"]) @ ["of"]))) @ ["The"]
 * (inorder Lf @ (["wood"] @ (([] @ ["Birnam"]) @ ["of"]))) @ ["The"]
 * (inorder Lf @ (["wood"] @ (["Birnam"] @ ["of"]))) @ ["The"]
 * (inorder Lf @ (["wood"] @ ["Birnam", "of"]) @ ["The"]
 * (inorder Lf @ ["wood", "Birnam", "of"]) @ ["The"]
 * ["wood", "Birnam", "of"] @ ["The"]
 * ["wood", "Birnam"] @ ["of", "The"]
 * ["wood"] @  ["Birnam", "of", "The"]
 * ["wood", "Birnam", "of", "The"]
 *
 * and for inord (tree, [])
 *
 * Br("The",Br("wood", Lf, Br("of", Br("Birnam", Lf, Lf), Lf)),Lf)
 * => inord(Br("wood", Lf, Br("of", Br("Birnam", Lf, Lf), Lf)), "The"::inord Lf)
 * => inord(Br("wood", Lf, Br("of", Br("Birnam", Lf, Lf), Lf)), ["The"])
 * => inord(Lf, "wood"::inord(Br("of", Br("Birnam", Lf, Lf), Lf), ["The"]))
 * => inord(Lf, "wood"::inord(Br("Birnam", Lf, Lf), "of"::inord(Lf, ["The"])))
 * => inord(Lf, "wood"::inord(Br("Birnam", Lf, Lf), ["of","The"]))
 * => inord(Lf, "wood"::inord(Lf, "Birnam"::inord(Lf, ["of","The"])))
 * => inord(Lf, "wood"::inord(Lf, ["Birnam", "of","The"]))
 * => inord(Lf, ["wood", "Birnam", "of","The"])
 * => ["wood", "Birnam", "of","The"]        *)

(* 4.20: Corrected. Intuition: simply swap t1 and t2 in pre-, in- and post-
 * definitions to see the following. Leaves are empty lists and so trivial/the
 * same across all cases. That is exactly what reflect is doing but now you are
 * building a list at the same time.
 *
 * Intuition:
 * ---------
 *      postorder (Br(x, xl, xr)) = postorder xl @ postorder xr @ [x]
 * If the tree has been reflected, simply swap xl and xr (recursively) so at
 * each level,
 *      postorder (reflect t) = postorder xr @ postorder xl @ [x]
 *                            = rev (preorder t)
 *
 * Similarly for inorder:
 * ---------------------
 *      inorder (Br(x, xl, xr)) = inorder xl @ [x] @ inorder xr
 *  So...
 *      inorder (reflect t) = inorder xr @ [x] @ inorder inorder xl
 *                          = rev (inorder t)
 * And lastly for preorder:
 * -------------------------
 *      preorder (Br(x, xl,xr)) = [x] @ preorder xl @ preorder xr
 * Hence...
 *      preorder (reflect t) = [x] @ preorder xr @ preorder xl
 *                           = rev (postorder t).   *)

(* 4.21: fun postorder t = rev (preorder (reflect t)); *)

(* 4.22: Questions are getting tricky. My solution: adaptation of 2013/1993
 * attempt without map. Should have been more inventive. It's irritating how
 * concise some of these online solutions are. *)

use "working-programmer/examples/sample3-sets.sml";

fun pre []      = [Lf]
  | pre (x::xs) = 
   (* Split a list into all possible ordered sublists for preorder. *)
    let fun split []      = [([], [])]
          | split (y::ys) =
            let fun cons (z, [])        = [([z], [])]
                  | cons (z, (y,v)::ys) = (z::y, v)::cons(z, ys) 
            in  ([], y::ys)::cons(y, split ys) end;

    (* Using above sublists, recursively solve and then merge solutions. *)
        fun solns []          = []
          | solns ((y,z)::ys) =
          let val lrs = cartprod (pre y, pre z)
              fun next []          = []
                | next ((p,q)::ps) = Br(x, p, q)::next ps
        in next lrs @ solns ys end
    in solns (split xs) end;

(* Online solution for study - they make heavy use of numbers for list
 * manipulations. Almost as good as Stella Lau's solution. *)
fun allpre []      = [Lf]
  | allpre (x::xs) =
     let fun joinx [] = []
           | joinx ((t1,t2)::pairs) = Br(x,t1,t2) :: joinx pairs
         fun step i = joinx (cartprod (allpre(List.take(xs,i)), 
                                       allpre(List.drop(xs,i))))
         fun build 0 = []
           | build i = step(i-1) @ build(i-1)
     in  build (1 + length xs)  end;

(* 4.23: Don't know. A constructor is function so it should work. But a
 * constructor needs to be given arguments whenever it is used so in this
 * case (online solution -) Br and Lf would only be usable as values. *)

(* 4.24: Let "Egypt"=1, "France"=2, "Hungary"=3, "Japan"=4, "Mexico"=5.
 * Sequences: [1,2,3,4,5], [5,4,3,2,1],
 *            [1,5,4,3,2], [5,1,2,3,4], 
 *            [5,1,4,2,3], [1,5,2,4,3]. *)

(* 4.25: *)
use "working-programmer/examples/DICTIONARY.sig";

structure DictList : DICTIONARY =
  struct
  type key  = string
  type 'a t = (key * 'a) list
  exception E of key
  val empty = []
  fun lookup ([], b)        = raise E b
    | lookup ((a,x)::xs, b) =
      case String.compare (a,b) of
            LESS    => lookup (xs, b)
          | EQUAL   => x
          | GREATER => raise E b;

  fun insert ([], b, y)        = [(b,y)]
    | insert ((a,x)::xs, b, y) =
      case String.compare (a,b) of 
           LESS    => (a,x)::insert (xs, b, y)
         | EQUAL   => raise E b
         | GREATER => (b,y)::(a,x)::xs;

  fun update ([], b, y)        = [(b,y)]
    |  update ((a,x)::xs, b, y) =
      case String.compare (a,b) of 
           LESS    => (a,x)::insert (xs, b, y)
         | EQUAL   => (b,y)::xs
         | GREATER => (b,y)::(a,x)::xs;
  end;

(* 4.26: *)
fun makearray (x,0) = Lf
  | makearray (x,1) = Br(x, Lf, Lf)
  | makearray (x,n) =
    let val (k, n') = ((n-1) div 2, (n-1)  mod 2)
    in  Br(x, makearray (x, k+n'), makearray (x, k)) end;

(* 4.27: *)
fun braunlist Lf            = []
  | braunlist (Br(x,xl,xr)) = 
    let val (left, right) = (braunlist xl, braunlist xr)
    in  x::inter(left, right) end;

(* 4.28: By labels, it can mean any value. Took a quick look at structure of
 * online soltuions in order to understand what question wanted. *)
fun sub (Lf, _)            = raise Subscript
  | sub (Br(x, xl, xr), k) =
    if k = 1 then x
    else if k mod 2 = 0
         then sub (xl, k div 2)
         else sub (xr, k div 2);

(* Corrected Lf clause. *)
fun update (Lf, k, w)            =
    if k = 1 then Br(SOME w, Lf, Lf)
    else if k mod 2 = 0
         then Br(NONE, update (Lf, k div 2, w), Lf)
         else Br(NONE, Lf, update (Lf, k div 2, w))
  | update (Br(x, xl, xr), k, w) =
    if k = 1 then Br(SOME w, xl, xr)
    else if k mod 2 = 0
         then Br(x, update (xl, k div 2, w), xr)
         else Br(x, xl, update (xr, k div 2, w));

(* 4.29:
 * Heap.empty = Lf
 * 4:       4
 * 
 * 2:       4
 *         / .
 *        2
 *
 * 6:       2
 *         / \
 *        6   4
 *
 * 1:       1
 *         / \
 *        2   6
 *       / .
 *      4
 *
 * 5:       1
 *         / \
 *        5   2
 *       / . / .
 *      6   4
 *
 * 8:       1
 *        /  \
 *      2     5
 *     / \   / .
 *    8   4 6
 *
 * 5:       1
 *        /  \
 *      5     2
 *     / \   / \
 *    5   6 8   4 *)

(* 4.30: The binary number is built up in reverse based on the directions to
 * the destination node. Start with an empty string. Left is prepend 0, right
 * is prepend 1. The destination node itself is prepending 1. This works
 * because we base the direction in which to go on the remainder we get when
 * we divide by 2, which is exactly the same process used when converting a
 * number to binary.
 *
 * For any heap, a node of i has left child 2i and right child 2i+1.
 * Converting this to binary is left is append 0 and right is append 1. *)

(* 4.31: Similar to Braun.sub and update with differences discussed above. *)
use "working-programmer/examples/ARITH.sig";
use "working-programmer/examples/Bin.sml";

fun func_array_sub (tree, n) =
  let fun sub (Lf, _)                = raise Subscript
        | sub (Br(x, xl, xr), [])    = x
        | sub (Br(x, xl, xr), 0::ys) = sub (xl, ys)
        | sub (Br(x, xl, xr), 1::ys) = sub (xr, ys)
  in  sub (tree, Bin.fromInt n) end;

fun func_array_update (tree, n, y) =
  let fun update (Lf, _)             = raise Subscript
        | update (Br(x, xl, xr), [])    = Br(y, xl, xr)
        | update (Br(x, xl, xr), 0::ys) = update (xl, ys)
        | update (Br(x, xl, xr), 1::ys) = update (xr, ys)
  in  update (tree, Bin.fromInt n) end;

(* I quote, the online solution: "This is something of a joke [...]"
 * Well, we can see that my solution's storage requirements can be quite high
 * but could be faster because the directions can be precalculated in one go. *)
fun left n = if n>3 then left (n div 2) else (n=2);
fun chop n = if n>3 then 2*chop(n div 2) + n mod 2 else 1;

(* 4.32: Online solution is probably more scalable for multiple operators of the
 * same precedence, which as it rightly points out, cannot always be grouped
 * arbitrarily. *)

use "working-programmer/examples/sample4-prop.sml";

fun show (Atom a)                         = a
  | show (Neg (Atom p))                   = "~"^p
  | show (Neg p)                          = "~("^show p^")"
  | show (Conj(p as Disj _, r as Disj _)) = "("^ show p^") & ("^show r^")"
  | show (Conj(p as Disj _, r))           = "("^show p^") & "^ show r
  | show (Conj(p, q as Disj _))           = show p^" & ("^show q^")"
  | show (Conj(p,q))                      = show p^" & "^ show q
  | show (Disj(p,q))                      = show p^" | "^ show q;

(* 4.33: *)
fun eval (Atom a, truelist)     = a mem truelist
  | eval (Neg p, truelist)      = not (eval (p, truelist))
  | eval (Conj (p,q), truelist) =
      eval (p, truelist) andalso eval (q,truelist)
  | eval (Disj (p,q), truelist) =
      eval (p, truelist) orelse  eval (q, truelist);

(* 4.34: Conj = list of lists, Disj = list. Correct according to solutions, but
 * testing output on nnf's show differently. *)
fun app (x, [])    = []
  | app (x, y::ys) = (x@y)::app(x,ys)

fun ldist ([], ys)    = []
  | ldist (x::xs, ys) = app(x,ys)@ldist(xs,ys);

fun lcnf (Conj(p, q)) = lcnf p @ lcnf q
  | lcnf (Disj(p, q)) = ldist (lcnf p, lcnf q)
  | lcnf p            = [[p]]; 

(* 4.35: *)
fun distrib (Conj(p,q), Conj(r,s)) =
    Conj(Conj(distrib (p,r), distrib (p,s)),
         Conj(distrib (q,r), distrib (q,s)))
  | distrib (p, Conj(q,r))         = Conj(distrib (p,q), distrib(p,r))
  | distrib (Conj(q,r), p)         = Conj(distrib (p,q), distrib(p,r))
  | distrib (p,q)                  = Disj(p,q);

(* 4.36: Intial attempt - DeMorgan's Laws. Once the proposition is in CNF,
 * apply DeMorgans', and check the tautology of its CNF.
 *
 * Online solutions say to simply swap Disj and Conj in distrib and following
 * functions. *)

fun dnf' (Conj(p,q)) = Neg (Disj(dnf' p, dnf' q))
  | dnf' (Disj(p,q)) = Neg (Conj(dnf' p, dnf' q))
  | dnf' p           = Neg p;

(* Online solution. *)
fun dnfdis (Disj(p,q), Conj(r,s)) =
    Disj(Conj(dnfdis (p,r), dnfdis (p,s)),
         Disj(dnfdis (q,r), dnfdis (q,s)))
  | dnfdis (p, Disj(q,r))         = Conj(dnfdis (p,q), dnfdis(p,r))
  | dnfdis (Disj(q,r), p)         = Conj(dnfdis (p,q), dnfdis(p,r))
  | dnfdis (p,q)                  = Conj(p,q);

fun dnf (Disj(p,q)) = Conj(dnf p, dnf q)
  | dnf (Conj(p,q)) = dnfdis (dnf p, dnf q)
  | dnf p           = p;

(* Testing code for 4.34 *)
fun implies (p, q) = Disj(Neg p, q);

val rich    = Atom "rich"
and landed  = Atom "landed"
and saintly = Atom "saintly";

val asm1 = implies (landed, rich)
and asm2 = Neg(Conj(saintly, rich))
and concl = implies (landed, Neg saintly);

val goal  = implies (Conj(asm1, asm2), concl);
show goal;

val cgoal  = cnf (nnfpos goal)
val cgoal_is_true = taut cgoal

(* DNF *)
fun dnfdis (p, Disj(q,r)) = Disj(dnfdis(p,q), dnfdis(p,r))
  | dnfdis (Disj(q,r), p) = Disj(dnfdis(q,p), dnfdis(r,p))
  | dnfdis (p, q) = Conj(p,q)   (*no disjunctions*) ;

fun dnf (Disj(p,q)) = Disj (dnf p, dnf q)
  | dnf (Conj(p,q)) = dnfdis (dnf p, dnf q)
  | dnf p = p    (*a literal*) ;

exception NonDNF;

fun dnfpos (Atom a)      = [a]
  | dnfpos (Neg(Atom _)) = []
  | dnfpos (Conj(p,q))   = dnfpos p @ dnfpos q
  | dnfpos  _            = raise NonDNF;

fun dnfneg (Atom _)      = []
  | dnfneg (Neg(Atom a)) = [a]
  | dnfneg (Conj(p,q))   = dnfneg p @ dnfneg q
  | dnfneg  _            = raise NonDNF;

fun dnftaut (Disj(p,q)) = dnftaut p andalso dnftaut q
  | dnftaut p = not (null (inter (dnfpos p, dnfneg p)));

(* Inconsistent goal *)
val incons_goal = Neg goal;

(* My attempt *)
val d'goal = cnf (nnfpos (dnf' (incons_goal)));
val d'goal_is = taut d'goal;

(* Online solution suggestion. *)
val dgoal  = dnf (nnfpos (incons_goal))
val dgoal_is_true  = dnftaut dgoal
