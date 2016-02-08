(*2007 Paper 1 Question 5
-------------------------
Binary Trees
------------------------- *)
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
