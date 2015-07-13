functor TreeFunctor ( ) : TREE =
  struct
  datatype 'a tree = Lf  |  Br of 'a * 'a tree * 'a tree;

  fun size Lf = 0
    | size (Br(v,t1,t2)) =  1 + size t1 + size t2;

  fun depth Lf = 0
    | depth (Br(v,t1,t2)) =  1 + Int.max (depth t1, depth t2);

  fun reflect Lf = Lf
    | reflect (Br(v,t1,t2)) = 
	  Br(v, reflect t2, reflect t1);

  fun preord (Lf, vs) = vs
    | preord (Br(v,t1,t2), vs) =
	  v :: preord (t1, preord (t2, vs));

  fun inord (Lf, vs) = vs
    | inord (Br(v,t1,t2), vs) =
	  inord (t1, v::inord (t2, vs));

  fun postord (Lf, vs) = vs
    | postord (Br(v,t1,t2), vs) =
	  postord (t1, postord (t2, v::vs));

  fun balpre  []    = Lf
    | balpre(x::xs) =
	let val k = length xs div 2
	in  Br(x, balpre(List.take(xs,k)),  balpre(List.drop(xs,k)))
	end;

  fun balin [] = Lf
    | balin xs =
	let val k = length xs div 2
	    val y::ys = List.drop(xs,k)
	in  Br(y, balin (List.take(xs,k)), balin ys)
	end;

  fun balpost xs = reflect (balpre (rev xs));
  end;
