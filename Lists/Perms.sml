fun perms [x] = [[x]]
  | perms ys = List.concat(map (fn y => map (fn xs => y::xs) (rest y ys)) ys)
and rest x ys = perms(List.filter (fn y => x<>y) ys);

(* Alternative solutions *)
fun insertall x [] = [[x]]
  | insertall x (y::ys) = (x::y::ys)::map (fn zs => y::zs) (insertall x ys);

fun perms [] = [[]]
  | perms (x::xs) = List.concat(map (insertall x) (perms xs));

(* Or, append heavy *)
fun perm [] = [[]]
  | perm xs =
  let
    (* Basically, splits *)
    fun select (ys, []) []
      | select (ys, x::xs) = (map (fn zs => x::zs) (perm (ys@xs))@select (x::ys))
  in
    select ([], xs)
  end;
