functor FlexArray (Braun: BRAUN) : FLEXARRAY = 
  struct
  datatype 'a array = Array of 'a Braun.Tree.tree * int;

  val empty = Array(Braun.Tree.Lf,0);

  fun length (Array(_,n)) = n;

  fun sub (Array(t,n), k) = 
      if 0<=k andalso k<n then Braun.sub(t,k+1) 
      else raise Subscript;

  fun update (Array(t,n), k, w) = 
      if 0<=k andalso k<n then Array(Braun.update(t,k+1,w), n)
      else raise Subscript;

  fun loext (Array(t,n), w) = Array(Braun.loext(t,w), n+1);

  fun lorem (Array(t,n)) = 
      if n>0 then Array(Braun.lorem t, n-1)
      else raise Size;

  fun hiext (Array(t,n), w) = Array(Braun.update(t,n+1,w), n+1);

  fun hirem (Array(t,n)) = 
      if n>0 then Array(Braun.delete(t,n) , n-1)
      else raise Size;

  end;
