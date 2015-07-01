(* 3.29: nextperm [2,3,1,4] = next ([2], [3,1,4])
 *                      => next ([3,2], [1,4])
 *                      => swap [3,2]
 *                      => 3::swap [2]
 *                      => 3::1::2::[4]
 *                      => [3,1,2,4]            *)

(* 3.30: If we look at [2,2,3,1] then replace <= with < would cause a loop. *)

(* 3.31: Match or empty (probably match because of the pattern matching in the
 * function clause. Add this clause - next (xs, []) = xs. *)

(* 3.32: 1 for the first and 500 for the second. *)

(* 3.33: I think the second one is more efficient because it requires constant
 * stack space since its recursive calls are iterative, despite number of
 * recursive calls being the same. *)

fun choose (0, xs) = []
  | choose (k, []) = []
  | choose (k, x::xs) =
    x::choose(k-1, xs) @ choose (k, xs);
