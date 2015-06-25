fun show s1 t1 r1 s2 t2 r2 q r =
  let
    val s' = s1 - q * s2
    val t' = t1 - q * t2
    fun ItS x = Int.toString x
  in
    print ("egciditer ((("
          ^(ItS s2)^", \t"
          ^(ItS t2)^"), \t"
          ^(ItS r2)^"), \t"
          ^(ItS s')^", \t"
          ^(ItS t')^"), \t"
          ^(ItS r)^"))\n")
  end;

fun egcd m n =
    let
      fun egcditer s1 t1 r1 s2 t2 r2 =
        let
          val q = r1 div r2
          val r' = r1 - q*r2
          val s' = s1 - q*s2
          val t' = t1 - q*t2
        in
          if r' = 0 then ((s2,t2),r2) else
            (show s1 t1 r1 s2 t2 r2  q r';
            egcditer s2 t2 r2 s' t' r')
          end
    in
      egcditer 1 0 m 0 1 n
    end;
