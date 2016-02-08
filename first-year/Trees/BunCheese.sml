(* 2011, Paper 1, Q2, not bad *)

datatype 'a tree = Leaf of 'a | Br of 'a tree * 'a tree;

fun bun (x, Leaf y) = (x=y, Leaf y)
  | bun (x, Br(t1, t2)) = 
  case bun (x,t1) of 
       (true, y) => (true, y)
     | (false, y) =>
         let val (r, t) = bun (x,t2)
         in  (r, Br(t1, t)) end;

fun cheese (x,t) = 
    case bun (x,t) of
         (true,y) => Leaf x
       | (false, y) => y;

val test = Br(
            Br(
                Br(Leaf 1, Leaf 2),
                Br(Leaf 4, Leaf 5)),
            Br(
                Br(Leaf 7, Leaf 8),
                Br(Leaf 0, Leaf 9)));

val result1 = cheese(6, test);
val result2 = cheese(8, test);

(* Larry's way *)
fun L_bun (x, Leaf y) = (x=y)
  | L_bun (x, Br(t1,t2)) = (bun t1) orelse (bun t2);

fun L_cheese (x,t) = if bun (x,t) then Leaf x else t;
