(* Dhruv Makwana, 2015 CST1A
* 1993, Paper 1, Question 8 *)
datatype T = n of int | d of T*T;
exception Empty;

fun flatten a =
    let
      fun flat (n(x)) xs = x::xs
        | flat (d(t1,t2)) xs = flat t1 (flat t2 xs)
    in 
      flat a []
    end;

fun nsplit xs =
let
  fun nsp _ [] = raise Empty
    | nsp xs [y] = [(xs, [y])]
    (* You can do 'let val in end' here to save double effort *)
    | nsp xs (y::ys) = (xs@[y], ys)::(nsp (xs@[y]) ys)
in
  nsp [] xs
end;

(* Gabor/James magic) *)
fun splits [] = raise Empty
  | splits [x] = []
  | splits (x::xs) = map (fn (a,b) => (x::a, b)) (([],xs)::(splits xs));

(* My very long solution...
 * Add t1 to the front of all the trees in ts *)
fun zip t1 [] = []
  | zip t1 (t::ts) =
  let
    fun z t1 [] r = r
      | z t1 (t::ts) r = z t1 ts ((d(t1,t))::r)
  in
    z t1 (t::ts) []
  end;

(* Insert t3 into all possible places in the given tree *)
fun ins t3 (n x) = [d(n x, t3)]
  | ins t3 (d(t1,t2)) = (d(d(t1,t2),t3))::zip t1 (ins t3 t2);

(* Prepend all *)
fun pall(y,e) = map ((ins o n) y) e;

(* Probably better to use List.concat if I'd know about it
* at the time *)
fun flat xs = 
  let
    fun fl [] _ = raise Empty
      | fl [[]] r = r
      | fl (y::ys) r =
      case y of [] => fl ys r
         | (z::zs) => fl (zs::ys) (z::r)
  in
    fl xs []
  end;

fun alltrees xs = foldl (flat o pall) [(n o hd) xs] (tl xs); 

(* Printing stuff -------------------------------------- *)
fun corr a = if String.size(a) > 1 then " "^a else a;
fun its x = Int.toString(x);
val cri = corr o its;

fun show (n(x)) = its x
  | show (d(n(x),n(y))) = "("^(its x)^(cri y)^")"
  | show (d(n(x), t)) = "("^(its x)^(show t)^")"
  | show (d(t, n(x))) = "("^(show t)^(its x)^")"
  | show (d(t1,t2)) = "("^(show t1)^(show t2)^")";

fun break b = b^"\n";
val format = map (print o break o show);

val allt = format o alltrees;

(* Alternatives: --------------------------------------- *)
(* Cartesian product *)
fun cart xs ys = flat (map (fn y => map (fn x => (x,y)) xs) ys);

(* Generally accepted solution *)
fun btrees [x] = [n x]
  | btrees xs = (flat o map pairlist o splits) xs
and pairlist (xs, ys) = map (fn(x,y) => d(x,y)) (cart (btrees xs) (btrees ys));
val bt = format o btrees;

(* Silly way, for comparison with Stella's solution
* It's btrees expanded all the way *)
fun ctrees [x] = [n x]
  | ctrees xs = flat (map (fn(xs,ys) => map (fn(x,y) => d(x,y)) (flat (map (fn y => map (fn x => (x,y)) (ctrees xs)) (ctrees ys)))) (splits xs));
val ct = format o ctrees;

(* Triple flat, utterly useless and slower than flat o flat *)
fun fflat xs =
let
  fun ffl [] _ = raise Empty
    | ffl [[]] _ = raise Empty
    | ffl [[[]]] r = r
    | ffl (y::ys) r =
    case y of [] => raise Empty
       | [[]] => ffl ys r
       | ([]::zss) => ffl (zss::ys) r
       | ((z::zs)::zss) => ffl ((zs::zss)::ys) (z::r)
in
  ffl xs []
end;

(* Stella's way *)
fun strees [x] = [n x]
  | strees xs = flat (flat (map (fn (a,b) => map (fn b => map (fn a => d(a,b)) (strees a)) (strees b)) (splits xs))); 
val st = format o strees;

(* And with triple flat *)
fun smtrees [x] = [n x]
  | smtrees xs = fflat (map (fn (a,b) => map (fn b => map (fn a => d(a,b)) (strees a)) (strees b)) (splits xs)); 
val smt = format o smtrees;

(* Quick testing: q? n *)
fun make i = List.tabulate(i, fn i => i+1);
val qa = (allt o make);
val qb = (bt o make);
val qc = (ct o make);
val qs = (st o make);
val qm = (smt o make);
