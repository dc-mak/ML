(* 2005, Paper 1, Question 6 *)
(* Dhruv Makwana *)
(* TODO: Change all code to work with Tw a  = Br (a, Lf, Lf) *)

(* Preamble for Lazy Lists *)
use "Useful.sml";

exception NoPath;

(* Single path *)
fun find_path p t = 
let
  fun find (Tw a) sum path =
    if (p (sum+a)) then rev (a::path) else raise NoPath
    | find (Bv(a,tl,tr)) sum path =
    let
      val sum2 = sum+a
      val path2 = a::path
    in
      if (p sum2) then rev path2
      else find tl sum2 path2
      handle NoPath => find tr sum2 path2
    end
in
  find t 0 []
end;

(* All paths, corrected for clarity *)
fun all_paths p t =
let
  fun find (Tw a) sum path result =
    if (p (sum+a)) then (rev (a::path))::result else result
    | find (Bv(a,tl,tr)) sum path result =
    let
      val sum2 = sum+a
      val path2 = a::path
      val result2 = if (p sum2) then (rev path2)::result else result
    in
        find tr sum2 path2 (find tl sum2 path2 result2)
    end
in
  find t 0 [] [[]]
end;

(* Lazy paths *)
fun interq Nil yf = yf()
  | interq (Cons(x,xf)) yf = Cons(x, fn () => interq (xf()) yf);

fun all_pathq pred tree =
let
  fun find (Tw a) sum path rest =
    if pred a then Cons(rev (a::path), rest) else rest()
    | find (Bv(a, left, right)) sum path rest =
    let
      val sum2 = sum+a; val path2 = a::path
      val rstR = fn () => find right sum2 path2 rest
      val rstLR = fn () => find left sum2 path2 rest
    in
      if pred sum2 then Cons((print "called\n"; rev path2), rstLR) else rstLR()
    end
in
    find tree 0 [] (fn () => Nil)
end;

val test = all_pathq (fn x => (x mod 3) = 2) (twList (List.tabulate (20, fn i => 2*i + 1)));

(* Wrong, because paths are just presented lazily,
*  not evaluated lazily. *)
fun all_ps_q p t =
let
  fun find (Tw a) sum path =
    if (p (sum+a)) then Cons(rev (a::path), fn () => Nil) else Nil
    | find (Bv(a,tl,tr)) sum path =
    let
      val sum2 = sum+a
      val path2 = a::path
      val rest = inter (find tl sum2 path2) (find tr sum2 path2)
    in
      if (p sum2) then Cons(rev path2, fn () => rest) else rest
    end
in
  find t 0 []
end;
