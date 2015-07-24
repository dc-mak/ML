functor PriorityQueue(structure Item: ORDER
                      and       Tree: TREE) : PRIORITY_QUEUE = 
  let open Tree 
  in
    struct
    structure Item = Item;
    fun x <= y = (Item.compare(x,y) <> GREATER);

    abstype t = PQ of Item.t tree
    with

      val empty = PQ Lf;

      fun null (PQ Lf) = true
        | null  _      = false;

      fun min (PQ(Br(v,_,_))) = v;

      fun insert'(w, Lf) = Br(w, Lf, Lf)
        | insert'(w, Br(v, t1, t2)) =
            if w <= v then Br(w, insert'(v, t2), t1)
                  else Br(v, insert'(w, t2), t1);

      fun insert (w, PQ t) = PQ (insert' (w, t));

      fun leftrem (Br(v,Lf,_)) = (v, Lf)
        | leftrem (Br(v,t1,t2)) = 
            let val (w, t) = leftrem t1
            in  (w, Br(v,t2,t))  end;

      fun siftdown (w, Lf, _) = Br(w,Lf,Lf)
        | siftdown (w, t as Br(v,_,_), Lf) =
            if w <= v then Br(w, t, Lf)
                  else Br(v, Br(w,Lf,Lf), Lf)
        | siftdown (w, t1 as Br(v1,p1,q1), t2 as Br(v2,p2,q2)) =
            if w <= v1 andalso w <= v2 then Br(w,t1,t2)
            else if v1 <= v2 then Br(v1, siftdown(w,p1,q1), t2)
               (* v2 < v1 *) else Br(v2, t1, siftdown(w,p2,q2));

      fun delmin (PQ Lf) = raise Size
        | delmin (PQ (Br(v,Lf,_))) = PQ Lf
        | delmin (PQ (Br(v,t1,t2))) = 
            let val (w, t) = leftrem t1
            in  PQ (siftdown (w,t2,t))  end;

      fun heapify (0, vs) = (Lf, vs)
        | heapify (n, v::vs) =
            let val (t1, vs1) = heapify (n div 2, vs)
            val (t2, vs2) = heapify ((n-1) div 2, vs1)
            in  (siftdown (v,t1,t2), vs2)  end;

      fun fromList vs = PQ (#1 (heapify (length vs, vs)));

      fun toList (d as PQ (Br(v,_,_))) = v :: toList(delmin d)
        | toList _ = [];

      fun sort vs = toList (fromList vs);

      end
    end
  end;
