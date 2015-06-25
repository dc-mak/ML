(* Dhruv Makwana, Trinity College, dcm41 *)
fun choose (n, xs) =
let 
  fun nCr 0 ys = [[]]
    | nCr n [] = []
    | nCr n (x::xs) = 
    map (fn ys => x::ys) (nCr (n-1) xs) @ nCr n xs
in
  nCr n xs
end;

choose (3, [1,2,3,4,5]);
(*
 * val choose = fn: int * 'a list -> 'a list list
 *)
