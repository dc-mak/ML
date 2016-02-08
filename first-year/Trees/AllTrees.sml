(* Dhruv Makwana *)
(* 2013, Lists and Trees *)
datatype 'a trees = Lf | Br of 'a * 'a trees * 'a trees;

fun cross (v,t1s,t2s) =
  List.concat(map (fn t1 => map (fn t2 => Br(v,t1,t2)) t2s) t1s);

(* Should be fine with [[]] and List.concat here but later... *)
fun splitmap f g xs [] = []
  | splitmap f g xs (y::ys) =
    rev ((f (y, g (rev xs), g ys))::splitmap f g (y::ys) ys);

fun trees [] = [Lf]
  | trees xs =
    List.concat(splitmap (cross) (trees) [] xs);
