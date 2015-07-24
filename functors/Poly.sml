signature POLY =
  sig
  include ARITH
  val quorem : t * t -> t * t
  val rem    : t * t -> t
  end;

functor Poly (Num : NUMBER) :> POLY where type t' = (Num.t * Num.t) list =
  struct
  (* Auxilliary *)
  val ~   = Num.neg
  val op+ = Num.sum
  val op- = Num.diff
  val op* = Num.prod
  val op/ = Num.divd

  fun op> (x,y) = Num.compare (x,y) = GREATER
  fun op< (x,y) = Num.compare (x,y) = LESS

  infix ==
  fun x == y = Num.eq (x,y)
  
  (* Signature related *)
  type t'    = (Num.t * Num.t) list
  datatype t = Poly of t'
  val zero           = Poly []

  fun make xs        = Poly xs
  fun dest (Poly xs) = xs

  (* neg and helper function negate *)
  fun negate xs = map (fn(m,a)=> (~m, ~a)) xs
  fun neg (Poly xs) = Poly (negate xs)

  (* sum and helper function add *)
  fun add ([], us)               = us
    | add (ts, [])               = ts
    | add ((m,a)::ts, (n,b)::us) =
	     if m>n then (m,a) :: add (ts, (n,b)::us)
	else if n>m then (n,b) :: add (us, (m,a)::ts)
	else (*m=n*) 
	     if a+b == Num.zero then add (ts, us)
			                else (m, a+b) :: add (ts, us)
  
  fun sum (Poly xs, Poly ys) = Poly (add (xs, ys))

  fun termprod ((m,a), xs) = map (fn(n,b)=> (m+n, a*b)) xs

  (* diff and helper function sub *)
  fun sub ([], us)               = negate us
    | sub (ts, [])               = ts
    | sub ((m,a)::ts, (n,b)::us) =
	     if m>n then (m,a) :: sub (ts, (n,b)::us)
	else if n>m then (n,b) :: sub (us, (m,a)::ts)
	else (*m=n*) 
	     if a+b == Num.zero then sub (ts, us)
                            else (m, a-b) :: sub (ts, us);

  fun diff (Poly xs, Poly ys) = Poly (sub (xs, ys))

  (* prod and helper function, based on mergesort variant for effieciency *)
  fun times (0, [], us)    = ([], [])
    | times (1, r::ts, us) = (termprod (r, us), ts)
    | times (n, ts, us)    =
      let val (l1, ts1) = times ((Int.+(n,1)) div 2, ts, us)
          val (l2, ts2) = times (n div 2, ts1, us)
      in  (add (l1,l2), ts2) end

  fun prod (Poly xs, Poly ys) = 
      let val x   = length xs
          val y   = length ys
          val arg = if x <= y then (x, xs, ys) else (y, ys, xs)
          val (l, _) = times arg
      in  Poly l end

  (* divd and other functions qr and rem as extras *)
  exception Undefined
  fun qr (ts, [])        = raise Undefined
    | qr (ts, (n,b)::us) =
    let fun divng ([],        qs) = (rev qs, [])
          | divng ((m,a)::ts, qs) =
            if m<n then (rev qs, (m,a)::ts)
            else divng  (add (ts, termprod ((m-n, ~a/b), us)),
                         (m-n, a/b) :: qs)
    in  divng (ts, [])  end;

  fun quorem (Poly ts, Poly us) = let val (x,y) = qr (ts,us)
                                  in  (Poly x, Poly y) end
  fun divd (ts,us) = #1(quorem (ts,us))
  and rem  (ts,us) = #2(quorem (ts,us));

  (* eq *)
  fun eq (Poly [], Poly [])                   = true
    | eq (Poly ((m,a)::ts), Poly ((n,b)::us)) =
        m == a andalso a == b andalso eq (Poly ts, Poly us)
    | eq _                                    = false

  (* toString and helper term *)
  fun term (m,a) =
      let
        val pow  = Num.toString m
        val coef = Num.toString a
      in
         if m == Num.zero then coef else
         if m == Num.one  then coef^"x"
                          else coef^"x"^pow
      end

  fun toString (Poly ts) = foldl (fn(x,e)=> e^term x) "" ts

  (* TODO: fix bad gcd algorithm - results are not reduced enough. *)
  fun gcd (Poly [], us) = us
    | gcd (ts, us) = gcd (rem (us,ts), ts);

  end;
