(* 7.1: O(n^2) because (1+2+...+n = n(n-1)/2). *)

(* 7.2: First one is simple and allows fast dequeing.
 *      Second one is also relatively simple and allows fast enqueing.
 *      Third one allows fast operations for both, but could have unexpected
 *      delays.
 *      First one would be faster in a situation where all items need to be
 *      dequeued in succession since it does not need to reverse them at any
 *      point. *)

(* 7.3: *)
structure Queue2a =
  struct
  type 'a t = 'a list
  exception E

  val empty = []

  fun enq (q,x) = x::q

  fun null (_::_) = false
    | null []     = true

  fun hd (x::_) = x
    | hd []     = raise E

  (* Was tempted to use List.last *)
  fun deq []      = raise E
    | deq [x]     = x
    | deq (_::xs) = deq xs

  end;

(* 7.4: Logarithmic in everything. *)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;
use "structures/Braun.sml";
use "structures/FLEXARRAY.sig";
use "structures/Flex.sml";

structure Queue4 =
  struct
  type 'a t = 'a Flex.array
  exception E
  val enq = Flex.hiext
  fun null q = (q = Flex.empty)
  fun deq q = if null q then raise E else Flex.lorem q
  end;

(* 7.5: They could work, if the size of the queue must be restricted. It would
 * be similar to Flex - lg n time for all operations. Better than 1,2 and 2a.
 * More predictable than 3 and 4. *)

(* 7.6: deq (enq (enq (enq (empty, 0), 1), 2)) for Q ([1,2],[]).
 *      enq (deq (enq (enq (empty, 2), 1)), 2) for Q ([1], [2]). *)

(* 7.7: *)
signature QUEUE = 
  sig
  type 'a t                          (* type of queues*)
  exception E                        (* for errors in hd, deq*)
  val empty  : 'a t                  (* the empty queue*)
  val enq    : 'a t * 'a -> 'a t     (* add to end*)
  val null   : 'a t -> bool          (* test for empty queue*)
  val hd     : 'a t -> 'a            (* return front element*)
  val deq    : 'a t -> 'a t          (* remove element from front*)
  val length : 'a t -> int           (* get number of items *)
  val equal  : ''a t * ''a t -> bool (* test queues for equality *)
  end;

structure Queue1 :> QUEUE = 
  struct
  type 'a t = 'a list;
  exception E;

  val empty = [];
  fun enq(q,x) = q @ [x];

  fun null(x::q) = false
    | null _ = true;

  fun hd(x::q) = x
    | hd [] = raise E;

  fun deq(x::q) = q
    | deq [] = raise E;

  val length = List.length
  (* Could have done val equal = op= but this allows for queue type only. *)
  fun equal (a, b) = (a=b)
  end;

structure Queue2 :> QUEUE = 
  struct
  datatype 'a t = empty 
        | enq of 'a t * 'a;
  exception E;

  fun null (enq _) = false
    | null empty = true;

  fun hd (enq(empty,x)) = x
    | hd (enq(q,x)) = hd q
    | hd empty = raise E;

  fun deq (enq(empty,x)) = empty
    | deq (enq(q,x)) = enq(deq q, x)
    | deq empty = raise E;

  fun len (empty, n)      = n
    | len (enq (q, x), n) = len (q, n+1)

  fun length q = len (q, 0)
  (* Could have done val equal = op= but this allows for queue type only. *)
  fun equal (a, b) = (a=b)
  end;

structure Queue3 :> QUEUE = 
  struct
  datatype 'a t = Queue of ('a list * 'a list);
  exception E;

  val empty = Queue([],[]);

  (*Normalized queue, if nonempty, has nonempty heads list*)
  fun norm (Queue([],tails)) = Queue(rev tails, [])
    | norm q = q;

  (*norm has an effect if input queue is empty*)
  fun enq(Queue(heads,tails), x) = norm(Queue(heads, x::tails));

  fun null(Queue([],[])) = true
    | null _ = false;

  fun hd(Queue(x::_,_)) = x
    | hd(Queue([],_)) = raise E;

  (*normalize in case heads become empty*)
  fun deq(Queue(x::heads,tails)) = norm(Queue(heads,tails))
    | deq(Queue([],_)) = raise E;

  fun length (Queue(a,b)) = List.length a + List.length b
  (* fun equal (Queue(a,b), Queue(c,d)) = (a @ rev b) = (c @ rev d) *)
  fun equal (Queue(a,b), Queue(c,d)) =
        let fun ac ([], [], [])      = true
              | ac ([], y, z)        = (y=z)
              | ac (x, [], z)        = (x=z)
              | ac (x::xs, y::ys, z) = (x=y) andalso ac (xs, ys, z)
            fun bd ([], [])          = ac (a, c, [])
              | bd ([], y)           = ac (a, c, rev y)
              | bd (x, [])           = ac (a, c, rev x)
              | bd (x::xs, y::ys)    = (x=y) andalso bd (xs,ys)
        in  bd (b,d) end
  end;

(* 7.8: *)
abstype 'a stack1 = S1 of 'a list 
  with
  val empty = S1 []
  
  fun push (S1 xs, x) = S1 (x::xs)

  fun pop (S1 [])      = S1 []
    | pop (S1 (x::xs)) = S1 xs

  fun top (S1 (x::xs)) = x

  fun size (S1 p) = List.length p

  fun null (S1 [])     = true
    | null (S1 (_::_)) = false
  end;

abstype 'a stack2 = Empty
                  | Push of 'a * 'a stack2
  with
  val empty = Empty
  and push  = Push

  fun pop Empty         = Empty
    | pop (Push (a,st)) = st

  fun top (Push (a,_)) = a

  fun size st =
    let fun sz (Empty, n)         = n
          | sz (Push (_, st'), n) = sz (st', n+1)
    in  sz (st, 0) end

  fun null Empty    = true
    | null (Push _) = false
  end;

(* 7.9: This is functionally useless, no way to have anything other than 0. *)
use "structures/ARITH.sig";
structure Rational =
  struct
  abstype t = Frac of int * int
    with
    val zero = Frac (0,1)
    fun gcd (0,n) = n
      | gcd (m,n) = gcd (n mod m, m);

    fun norm ((a,b)) =
      let
        val (x,y) = (Int.abs a, Int.abs b)
        val hcf   = gcd (x,y)
        fun sgn x =  if x < 0 then ~1 else 1
        val sign  = sgn a * sgn b
        val (p,q) = if sign = 1 then (x, y) else (~x, y)
      in
         (p div hcf, q div hcf)
      end
    fun sum  (Frac(a,b), Frac(x,y)) = Frac (norm (a*y + b*x, b*y))
    fun diff (Frac(a,b), Frac(x,y)) = Frac (norm (a*y - b*x, b*y))
    fun prod (Frac(a,b), Frac(x,y)) = Frac (norm (a*x, b*y))
    fun quo  (ab,        Frac(y,x)) = prod (ab, Frac(y,x))
    end
  end;

(* 7.10: Would declare a Month dataype for type safety instead of using strings.
 * Would then need a today function to convert month string to type month. 
 * Online solution deals only with numbers. I re-used code from Chapter 2. *)

abstype date = Date of int * string
  with
  exception NotDate
  local
    fun check_date (d, m) =
      (d > 0) andalso
      ((d <= 28 andalso  m = "February" )   orelse
       (d <= 30 andalso (m = "April"        orelse
                         m = "June"         orelse
                         m = "September"    orelse
                         m = "November" ))  orelse
       (d <= 31 andalso (m = "January"      orelse
                         m = "March"        orelse
                         m = "May"          orelse
                         m = "July"         orelse
                         m = "August"       orelse
                         m = "October"      orelse
                         m = "December" )))

    fun predecessor "March"     = Date(28, "February")
      | predecessor "February"  = Date(31, "January")
      | predecessor "April"     = Date(31, "March")
      | predecessor "June"      = Date(31, "May")
      | predecessor "September" = Date(31, "August")
      | predecessor "November"  = Date(31, "October")
      | predecessor "May"       = Date(30, "April")
      | predecessor "July"      = Date(30, "June")
      | predecessor "August"    = Date(30, "July")
      | predecessor "October"   = Date(30, "September")
      | predecessor "December"  = Date(30, "November")
      | predecessor _           = raise NotDate

    fun successor "January"   = Date(1, "February")      
      | successor "February"  = Date(1, "March")
      | successor "March"     = Date(1, "April")
      | successor "April"     = Date(1, "May")
      | successor "May"       = Date(1, "June")
      | successor "June"      = Date(1, "July")
      | successor "July"      = Date(1, "August")
      | successor "August"    = Date(1, "September")
      | successor "September" = Date(1, "October")
      | successor "October"   = Date(1, "November")
      | successor "November"  = Date(1, "December")
      | successor _           = raise NotDate
  in
    fun today (d,m) = if check_date (d,m) then Date (d,m) else raise NotDate
    fun yesterday (Date(d,m)) =
      if check_date(d-1,m) then Date(d-1,m) else predecessor m
    fun tomorrow  (Date(d,m)) =
      if check_date(d+1,m) then Date(d+1,m) else successor m
  end
  end;

(* Online solution - so much better. *)
abstype date = D of int * int
  with
    local
      val months = [31,28,31,30,31,30,31,31,30,31,30,31];
      fun days m = List.nth (months, m-1);
      fun date_ok (d,m) =
      1 <= m andalso m <= 12 andalso 1 <= d andalso d <= days m;
    in
      exception Date;

      fun today (d,m) = if date_ok (d,m) then D(d,m) else raise Date;

      fun day   (D(d,m)) = d
      and month (D(d,m)) = m;

      fun tomorrow (D(d,m)) =
      if date_ok (d+1,m) then D(d+1,m)
      else if m<12 then D(1,m+1)
      else raise Date;

      fun yesterday (D(d,m)) =
      if date_ok (d-1,m) then D(d-1,m)
      else if m>1 then D(days (m-1), m-1)
      else raise Date;
    end
  end;

(* 7.11: See 7.3, structure Queue2a. *)

(* 7.12: Dunno how to get rid of eqtype that comes up. *)
structure NotQueue =
  struct
  type 'a t = 'a list
  exception E
  val empty             = [] : 'a t
  fun enq  (xs, x : 'a) = xs : 'a t
  fun null (b : 'a t)   = false
  fun deq   b           = b : 'a t
  fun hd   (b::bs)      = b : 'a
  end;

(* 7.13: Java interfaces/other languages with header files for ML signatures.
 *       Any OO language for functors. *)

(* 7.14: Not great, just enqueues then dequeues, real world would have both
 * intertwined, not one sequential bunch after another. *)

(* 7.15: *)
use "working-programmer/examples/sample7-Matrices.sml";
fun secl x f y = f (x,y);
fun secr f y x = f (x,y);

structure PathZSP' : ZSP =
  struct
  type t   = int option
  val zero = NONE           (* Infinity *)

  fun sum (NONE, y)         = y
    | sum (x, NONE)         = x
    | sum (SOME x, SOME y)  = SOME (Int.min (x,y))

  fun prod (SOME x, SOME y) = SOME (x+y)
    | prod _                = NONE
  end;

(* 7.16: Vectors, whoohoo! *)
functor VMatrixZSP (Z: ZSP) : ZSP =
  struct
  
  (* Make life easier. *)
  open Vector

  type t = Z.t vector vector

  val zero = fromList [] : t

  (* Choose to map over smaller of the two vectors.
   * Ensures sum and prod default to zero if one of the vectors is zero. *)
  fun pair_map f (av, bv) =
    let val (xv, yv) = if length av <= length bv then (av, bv) else (bv, av)
        fun apply (index, elem) = f (elem, sub (yv, index))
    in  mapi apply xv end

  fun sum (rowsA, rowsB) =
      pair_map (pair_map Z.sum) (rowsA, rowsB)

  fun dotprod pairs = foldl Z.sum Z.zero (pair_map Z.prod pairs)

  fun transp matrix =
    let val rows = length matrix
        val cols = length (sub (matrix, 0))
        fun elem (row, col) = sub(sub (matrix, row), col)
    in tabulate (cols,
        fn col => tabulate (rows,
          fn row => elem (row, col))) end

  fun prod (rowsA, rowsB) = 
    if length rowsA = 0 orelse length rowsB = 0 then zero else
    let val colsB = transp rowsB
    in  map (fn row => map (fn col => dotprod (row,col)) colsB) rowsA end
  end;

(* 7.17: *)
use "structures/DICTIONARY.sig";
use "working-programmer/examples/sample7-DictionaryPQs.sml";
functor DictList (Key: ORDER) : DICTIONARY = 
  struct
  type key = Key.t;
  abstype 'a t = L of (key * 'a) list
    with

    exception E of key;

    val empty = L [];

    fun lookup (L [], b)        = raise E b
      | lookup (L ((a,x)::xs), b) =
      (case Key.compare(a,b) of
               GREATER => lookup (L xs, b)
             | EQUAL   => x
             | LESS    => raise E b);

    fun ins ([], b, y)              = [(b,y)]
      | ins ((a,x)::xs, b, y)       =
          (case Key.compare(a,b) of
               GREATER => (a,x)::ins (xs, b, y)
             | EQUAL   => raise E b
             | LESS    => (b,y)::(a,x)::xs);

    fun insert (L xs, b, y) = L (ins (xs, b, y));

    fun upd ([], b, y)        = [(b,y)]
      | upd ((a,x)::xs, b, y) =
          (case Key.compare(a,b) of
               GREATER => (a,x)::upd (xs, b, y)
             | EQUAL   => (a,y)::xs
             | LESS    => (b,y)::(a,x)::xs);

    fun update (L xs, b, y) = L (upd (xs, b, y));
    end
  end;

(* 7.18: *)
signature PRIORITY_QUEUE =
  sig
  structure Item : ORDER
  type t
  val empty    : t
  val null     : t -> bool
  val insert   : Item.t * t -> t
  val min      : t -> Item.t
  val delmin   : t -> t
  val fromList : Item.t list -> t
  val toList   : t -> Item.t list
  val sort     : Item.t list -> Item.t list
  end;

functor PriorityQueue (Item : ORDER) : PRIORITY_QUEUE = 
  struct
  structure Item = Item;     (* required by signature *)
  fun x <= y = (Item.compare (x,y) <> GREATER);
  (* Would have been smarter to redefine tree constructors here. *)
  abstype t = T of Item.t tree
    with
    val empty = T Lf;

    fun null (T Lf) = true
      | null (T (Br _)) = false;

    fun min (T(Br(v,_,_))) = v;

    fun ins(w, Lf)            = Br(w, Lf, Lf)
      | ins(w, Br(v, t1, t2)) =
      if w <= v then Br(w, ins(v, t2), t1)
                else Br(v, ins(w, t2), t1);

    fun insert (w, T tr) = T (ins (w, tr));

    fun leftrem (Br(v,Lf,Lf)) = (v, Lf)
      | leftrem (Br(v,t1,t2)) = 
          let val (w, t) = leftrem t1
      in  (w, Br(v,t2,t))  end;

    fun siftdown (w, Lf, Lf) = Br(w,Lf,Lf)
      | siftdown (w, t as Br(v,Lf,Lf), Lf) =
          if w <= v then Br(w, t, Lf)
                    else Br(v, Br(w,Lf,Lf), Lf)
      | siftdown (w, t1 as Br(v1,p1,q1), t2 as Br(v2,p2,q2)) =
          if w <= v1 andalso w <= v2 then Br(w,t1,t2)
          else if v1 <= v2 then Br(v1, siftdown(w,p1,q1), t2)
             (* v2 < v1 *) else Br(v2, t1, siftdown(w,p2,q2));

    fun delete_min Lf = raise Size
      | delete_min (Br(v,Lf,_)) = Lf
      | delete_min (Br(v,t1,t2)) = 
          let val (w,t) = leftrem t1
      in  siftdown (w,t2,t)  end;

    fun delmin (T tr) = T(delete_min tr);

    fun heapify (0, vs) = (Lf, vs)
      | heapify (n, v::vs) =
      let val (t1, vs1) = heapify (n div 2, vs)
          val (t2, vs2) = heapify ((n-1) div 2, vs1)
      in  (siftdown (v,t1,t2), vs2)  end;

    fun fromList vs = T(#1(heapify (length vs, vs)));

    fun to_L (t as Br(v,_,_)) = v :: to_L(delete_min t)
      | to_L Lf = [];

    fun toList (T tr) = to_L tr;

    fun sort vs = toList (fromList vs);
  end
  end; 

(* 7.19: *)
functor ListPriority (Item : ORDER) : PRIORITY_QUEUE = 
  struct
  structure Item = Item;     (* required by signature *)
  fun x <= y = (Item.compare (x,y) <> GREATER);
  abstype t = L of Item.t list
    with

    val empty = L [];

    fun null (L [])     = true
      | null (L (_::_)) = false;

    fun ins ([], b)    = [b]
      | ins (a::xs, b) =
        if a <= b then b::a::xs else a::ins(xs,b);

    fun insert (b, L xs) = L (ins (xs,b));

    fun min (L (x::_)) = x;

    fun delmin (L []) = raise Size
      | delmin (L (_::xs)) = L xs;

    fun fromList xs = L(foldl (fn(x,e)=> ins(e,x)) [] xs);

    fun toList (L xs) = xs;

    val sort = toList o fromList;

    end
  end;

(* 7.20: For comparison, but not very modular in the long run. *)
functor Sorting (Item : ORDER) =
  struct
  local
    fun x <= y = (Item.compare (x,y) <> GREATER)

    fun merge ([], ys)       = ys
      | merge (xs, [])       = xs
      | merge (x::xs, y::ys) =
          if x <= y then x::merge (xs, y::ys)
                    else y::merge (x::xs, ys)

    fun quicker ([], sorted)    = sorted
      | quicker (a::bs, sorted) =
      let fun part (left, right, [])    =
              quicker (left, a::quicker (right,sorted))
            | part (left, right, x::xs) =
              if x <= a then part (x::left, right, xs)
                        else part (left, x::right, xs)
      in part ([], [], bs) end
  in
    fun mergesort []                 = []
      | mergesort [x]                = []
      | mergesort (xs : Item.t list) =
        let
          val k = length xs div 2
        in
          merge (mergesort (List.take (xs,k)),
                 mergesort (List.drop (xs,k)))
        end

    fun quicksort xs = quicker (xs, [])
  end
  end;

(* 7.21: *Like* order, but not exactly: t*t -> bool. *)
functor AssocList (type t
                   val compare : t*t -> order) : DICTIONARY = 
  struct
  type key = t;
  type 'a t = (t * 'a) list;
  exception E of key;

  val empty = [];

  fun lookup ([], b)           = raise E b
    | lookup ((a,x)::pairs, b) =
      (fn EQUAL => x | _ => lookup (pairs, b)) (compare (a,b))

  fun insert ([], b, y) = [(b,y)]
    | insert ((a,x)::pairs, b, y) =
      (fn EQUAL => raise E b
        | _     => (a,x)::insert (pairs, b,y)) (compare (a,b))

  fun update (pairs, b, y) = (b,y)::pairs

  end;

(* 7.22: Brief note for future reference -
 *  When *DEFINING* a functor, the argument/parameter list is a signature.
 *  When *USING* a functor, the argument is a structure (w/o struct...end). *)
functor AbsAssocList (eqtype key) : DICTIONARY = 
  struct
  type key = key;
  abstype 'a t = L of (key * 'a) list
    with
    exception E of key;

    val empty = L [];

    fun lookup (L [], b)             = raise E b
      | lookup (L ((a,x)::pairs), b) =
        if a=b then x else lookup(L pairs, b)

    fun insert (L [], b, y) = L [(b,y)]
      | insert (L ((a,x)::pairs), b, y) =
        if a=b then raise E b else
        (fn L xs => L ((a,x)::xs)) (insert(L pairs, b, y))

    fun update (L pairs, b, y) = L ((b,y)::pairs);
  end
  end;

(* 7.23: *)
signature PORDER =
  sig
  type t
  val compare : t * t ->  order option
  end;

functor LexPOrder (structure P1 : PORDER
                   structure P2 : PORDER) : PORDER =
  struct
  type t = P1.t * P2.t;
  fun compare ((x1,y1), (x2,y2)) =
      (case P1.compare (x1,x2) of 
            SOME EQUAL => P2.compare (y1,y2)
          | ord       => ord)
  end;

(* 7.24: Corrected so that (f a1 = f a2) does not export resultl (a1 = a2). *)
functor POrderMap (type t; structure PO: PORDER; val f : t -> PO.t) : PORDER =
 struct
 type t = t
 fun compare (a1, a2) =
     (fn SOME EQUAL => NONE | ord => ord ) (PO.compare (f a1, f a2))
 end;

(* 7.25: Either () or 'struct end'.
 *       Online solution - instance of empty signature . *)

(* 7.26: It should accept the first one and the second one easily enough.
 *       The third will be rejected because the type sharing is strong and
 *       traverses all levels of both signatures. *)

(* 7.27: *)
signature IN =
  sig
  structure PQueue : PRIORITY_QUEUE
  type problem
  val  goals : problem -> PQueue.t
  end;

signature OUT =
  sig
  structure PQueue : PRIORITY_QUEUE
  type solution
  val  solve : PQueue.t -> solution
  end;

functor Input (structure PQueue : PRIORITY_QUEUE) : IN =
  struct
  structure PQueue = PQueue
  type problem = char
  fun goals c = PQueue.empty
  end;

functor Output (structure PQueue : PRIORITY_QUEUE) : OUT =
  struct
  structure PQueue = PQueue
  type solution = string
  fun solve pq = "Done"
  end;

structure IntOrder : ORDER =
  struct
  type t = int
  val compare = Int.compare
  end;

structure IntPQueue = PriorityQueue (IntOrder);

structure Problems  = Input  (structure PQueue = IntPQueue);
structure Solutions = Output (structure PQueue = IntPQueue);

structure StrOrder : ORDER =
  struct
  type t = string
  val compare = String.compare
  end;

structure StrPQueue = PriorityQueue (StrOrder);

structure ProblemsNoShare  = Input  (structure PQueue = StrPQueue);
structure SolutionsNoShare = Output (structure PQueue = IntPQueue);

(* 7.28: Then there will be two priority queue structures whose types may or
 * may not match. As mentioned in the book, calling functors from inside
 * functors will create new structures and duplicate it.
 * Online solution - sharing constraint will be violated because new abstract
 * types never share. *)
functor Input2 (structure Ord : ORDER) : IN =
  struct
  structure PQueue = PriorityQueue (Ord)
  type problem = char
  fun goals c = PQueue.empty
  end;

functor Output (structure Ord : ORDER) : OUT =
  struct
  structure PQueue = PriorityQueue (Ord)
  type solution = string
  fun solve pq = "Done"
  end;

(* 7.29: Literally copied Queue3 and changed 'structure' to 'functor...()'. *)
functor QueueFunctor ( ) :> QUEUE = 
  struct
  datatype 'a t = Queue of ('a list * 'a list);
  exception E;

  val empty = Queue([],[]);

  (*Normalized queue, if nonempty, has nonempty heads list*)
  fun norm (Queue([],tails)) = Queue(rev tails, [])
    | norm q = q;

  (*norm has an effect if input queue is empty*)
  fun enq(Queue(heads,tails), x) = norm(Queue(heads, x::tails));

  fun null(Queue([],[])) = true
    | null _ = false;

  fun hd(Queue(x::_,_)) = x
    | hd(Queue([],_)) = raise E;

  (*normalize in case heads become empty*)
  fun deq(Queue(x::heads,tails)) = norm(Queue(heads,tails))
    | deq(Queue([],_)) = raise E;

  fun length (Queue(a,b)) = List.length a + List.length b
  (* fun equal (Queue(a,b), Queue(c,d)) = (a @ rev b) = (c @ rev d) *)
  fun equal (Queue(a,b), Queue(c,d)) =
        let fun ac ([], [], [])      = true
              | ac ([], y, z)        = (y=z)
              | ac (x, [], z)        = (x=z)
              | ac (x::xs, y::ys, z) = (x=y) andalso ac (xs, ys, z)
            fun bd ([], [])          = ac (a, c, [])
              | bd ([], y)           = ac (a, c, rev y)
              | bd (x, [])           = ac (a, c, rev x)
              | bd (x::xs, y::ys)    = (x=y) andalso bd (xs,ys)
        in  bd (b,d) end
  end;

(* 7.30: Corrected so that datatype constructors are visible through opaque
 *       signature and so making the structure usable. *)
signature SEQUENCE = 
  sig
  exception Empty
  type 'a seq
  val Cons       : 'a * (unit -> 'a seq) -> 'a seq
  val Nil        : 'a seq
  val cons       : 'a * 'a seq -> 'a seq
  val null       : 'a seq -> bool
  val hd         : 'a seq -> 'a
  val tl         : 'a seq -> 'a seq
  val fromList   : 'a list -> 'a seq
  val toList     : 'a seq -> 'a list
  val take       : 'a seq * int -> 'a list
  val drop       : 'a seq * int -> 'a seq
  val @          : 'a seq * 'a seq -> 'a seq
  val interleave : 'a seq * 'a seq -> 'a seq
  val map        : ('a -> 'b) -> 'a seq -> 'b seq
  val filter     : ('a -> bool) -> 'a seq -> 'a seq
  val iterates   : ('a -> 'a) -> 'a -> 'a seq
  val tabulate   : (int -> 'a) -> 'a seq
  val enumerate  : 'a seq seq -> 'a seq
  end;

functor SeqFunctor ( ) :> SEQUENCE =
  struct
  exception Empty;
  datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq)

  fun hd (Cons(x,xf)) = x
    | hd Nil = raise Empty;

  fun tl (Cons(x,xf)) = xf()
    | tl Nil = raise Empty;

  fun cons(x,xq) = Cons(x, fn()=>xq);

  fun null (Cons _) = false
    | null Nil      = true;

  (* Small change to make it lazier. *)
  fun fromList [] = Nil
    | fromList (x::xs) = Cons(x, fn()=> fromList xs);

  fun toList Nil = []
    | toList (Cons(x,xf)) = x :: toList (xf());

  fun take (xq, 0) = []
    | take (Nil, n) = raise Subscript
    | take (Cons(x,xf), n) = x :: take (xf(), n-1);

  fun drop (xq, 0) = xq
    | drop (Nil, n) = raise Subscript
    | drop (Cons(x,xf), n) = drop (xf(), n-1);

  infix 5 @;
  fun Nil @ yq = yq
    | (Cons(x,xf)) @ yq = Cons(x, fn()=> (xf()) @ yq);

  fun interleave (Nil,    yq) = yq
    | interleave (Cons(x,xf), yq) = 
	  Cons(x, fn()=> interleave(yq, xf()));

  (** functionals for sequences **)
  fun map f Nil  = Nil
    | map f (Cons(x,xf)) = Cons(f x, fn()=> map f (xf()));

  fun filter pred Nil = Nil
    | filter pred (Cons(x,xf)) =
	  if pred x then Cons(x, fn()=> filter pred (xf()))
		    else filter pred (xf());

  fun iterates f x = Cons(x, fn()=> iterates f (f x));

  fun tabulate f =
      let fun tab k = Cons(f k, fn () => tab (k+1)) in tab 0 end

  fun enumerate Nil                     = Nil
    | enumerate (Cons(Nil, xqf))        = enumerate (xqf())
    | enumerate (Cons(Cons(x,xf), xqf)) =
          Cons(x, fn()=> interleave(enumerate (xqf()), xf()));
  end;

(* Little more verbose with a proper queue. *)
functor SearchStrategies (structure Seq : SEQUENCE
                          structure Q   : QUEUE) =
  struct
  fun depthFirst next x =
      let fun dfs [] = Seq.Nil
            | dfs (y::ys) = Seq.Cons(y, fn()=> dfs (next y @ ys))
      in  dfs [x] end;

  fun breadthFirst next x =
      let
        fun update q = foldl (fn(x,e)=> Q.enq(e,x)) (Q.deq q) (next (Q.hd q))
        fun bfs q = if Q.null q then Seq.Nil
                    else Seq.Cons(Q.hd q, fn ()=> bfs (update q))
      in bfs (Q.enq(Q.empty, x)) end;
  end;

(* 7.31: QUEUE1:     list and bool.
 *       QUEUE2:     bool
 *       QUEUE3:     list and bool
 *       DICTIONARY: -
 *       FLEXARRAY:  int; but all are self contained. *)

(* 7.32: UsedTwice contains both the structures Item and Tree, signature
 *       matching takes care of the extra functions and so the same structure
 *       is used twice by different identifiers.
 *       Online solution - It works because they have no common names. *)

(* 7.33: Any operations in Queue2 that have the same name as those in Queue3
 *       will be inaccessible. *)

(* 7.34: Is it the 2.0? Wouldn't it default to normal real? (Yes.) *)
