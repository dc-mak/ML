(*2015, Paper 1, Q1*)
(* Dhruv Makwana *)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun altern xs =
let 
  fun alt [] result = rev result
    | alt [x] result = rev (x::result)
    | alt (x::y::xs) result = alt xs (x::result)
in
  alt xs []
end;

fun arrayoflist [] = Lf
  | arrayoflist [x] = Br(x, Lf, Lf)
  | arrayoflist (x::xs) =
  let
    val even = altern xs
    val odd = altern (tl xs)
  in
    Br(x, arrayoflist even, arrayoflist odd)
  end;

fun merge [] ys = ys
  | merge xs [] = xs
  | merge (x::xs) (y::ys) = 
  if x < y then x::merge xs (y::ys)
  else y::merge (x::xs) ys;


fun check pred arr =
let
  fun chk pred n Lf = []
    | chk pred n (Br(x, l, r)) =
    let
      val even = chk pred (2*n) l
      val odd = chk pred (2*n+1) r
      val result = merge even odd
    in
      if pred n then x::result else result
    end
in
  chk pred 1 arr
end;
