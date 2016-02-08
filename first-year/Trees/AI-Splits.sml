fun splits l =
  let fun spl (lef, []) = []
        | spl ([], x::xs) = spl([x], xs)
        | spl (lef, x::xs) = (rev lef, x::xs)::spl(x::lef,xs)
  in spl([],l)
  end;

fun concat [] = [] | concat (l::ls) = l @ concat ls;

fun cartesian (xs, ys) = concat (map (fn x => map (fn y => (x, y)) ys) xs);

datatype tree = n of int | d of tree * tree;

fun alltrees [x] = [n x] 
  | alltrees l =
      let val s = splits l
          fun comb (l1, l2) = map (fn (x, y) => d (x, y))
                                  (cartesian (alltrees l1, alltrees l2))
      in concat(map comb s)
      end;
