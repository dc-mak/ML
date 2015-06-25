fun quik([], sorted) = sorted
  | quik([x], sorted) = x::sorted
  | quik(a::bs, sorted) =
  let fun part (l, r, []) : real list =
  quik(l, a :: quik(r,sorted))
    | part (l, r, x::xs) =
    if x<=a then part(x::l, r, xs)
    else part(l, x::r, xs)
  in part([],[],bs) end;

quik([7.2, 1.3, 1.6, 6.3, 1.22, 3.61], []);
