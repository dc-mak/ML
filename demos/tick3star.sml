(* IA ML/Java demonstrator, Dhruv Makwana *)

datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

(* Q1 *)
fun insert (str, Lf)                         = Br (str, Lf, Lf)
  | insert (str, tree as Br(x, left, right)) =
    if str < x then Br(x, insert (str, left), right) else
    if str > x then Br(x, left, insert (str, right)) else tree

fun member (str, Lf)                 = false
  | member (str, Br(x, left, right)) =
      str = x orelse member (str, left) orelse member (str, right)

(* Q2 *)
fun union (tree, Lf) = tree
  | union (tree, Br(x, left, right)) = 
      union(union(insert(x, tree), left), right)
   (* union(union(insert(x, tree), right), left) *)
   (* union(insert(x, tree), union(left, right)) *)

(* Q3 *)
fun inter (tree, Lf) = Lf
  | inter (Lf, tree) = Lf
  | inter (Br(x, l, r), tree) =
    let val (left, right) = (inter (l, tree), inter(r, tree))
    in  if member (x, tree)
           then Br (x, left, right)
           else union (left, right) end

(* Q4 *)
fun remove (str, Lf) = Lf
  | remove (str, Br(x, left, right)) = 
    if x < str then Br(x, remove (str, left), right) else
    if x > str then Br(x, left, remove (str, right)) else
                    union (left, right)
