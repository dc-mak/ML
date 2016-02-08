(*2007 Paper 1 Question 6
* ----------------------
* Nothing special
* ---------------------- *)
datatype 'a meal = Snack of 'a
                 | Lunch of 'a meal * 'a meal
                 | Feast of 'a meal * 'a meal * 'a meal;

fun snack m = let

  val l = ref []
  fun munch (Snack x) = (l := x:: !l)
    | munch (Lunch (m1, m2)) = (munch m1; munch m2)
    | munch (Feast (m1, m2, m3)) = 
          (munch m1; munch m2; munch m3)

in munch m; !l end;

fun snacker m = let 

    fun mun(Snack x) = [x]
      | mun(Lunch (m1, m2)) = mun(m2)@mun(m1)
      | mun(Feast (m1, m2, m3)) =
            mun m3 @ mun m2 @ mun m1

  in mun m end;

fun gluttony [] m2 = []
  | gluttony m1 m2 = let

    val replace =
    case hd(m1) of 
         (Snack x)            => m2
       | (Lunch (x1, x2))     => hd(m1)
       | (Feast (x1, x2, x3)) => hd(m1)

  in replace::(gluttony (tl m1) m2) end;

fun glut k [] m2 = []
  | glut 0 m1 m2 = m1
  | glut k m1 m2 = let

      val (s, t) =
      case (k, hd(m1)) of
           (1, Snack x)            => (0, m2)
         | (1, Lunch (x1, x2))     => (1, hd(m1))
         | (1, Feast (x1, x2, x3)) => (1, hd(m1))
         | (_, Snack x)            => (k-1, hd(m1))
         | (_, Lunch (x1, x2))     => (k, hd(m1))
         | (_, Feast (x1, x2, x3)) => (k, hd(m1))

  in t::(glut s (tl m1) m2) end;
