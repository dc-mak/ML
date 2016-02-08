(*Bubble Sort*)
fun bubsort [] = []
  | bubsort lst = 
  let 
    fun bubble ([], _) = []
      | bubble ([s], _) = [s]
        (*because tl([]) raises Empty*)
      | bubble (x::xs, []) = x::xs
      | bubble (x::xs, y::ys) =
        if x > hd(xs) then
          bubble(hd(xs)::bubble(x::tl(xs), ys), y::ys)
        else
          bubble(x::bubble(xs, ys), ys)
  in
    bubble(lst, lst)
  end;
