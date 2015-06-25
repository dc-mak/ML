(* Sequences *)
datatype 'a seq = Nil | Cons of 'a * (unit -> 'a seq);
exception Nil_Sequence;

fun seqHd (Cons(x, xf)) = x | seqHd _ = raise Nil_Sequence;
fun seqTl (Cons(x, xf)) = xf() | seqTl Nil = Nil;
fun seqNth 0 (Cons(x,xf)) = x
  | seqNth n (Cons(x,xf)) = seqNth (n-1) (xf())
  | seqNth n Nil = raise Nil_Sequence;

fun seqMap f Nil = Nil
  | seqMap f (Cons(x, xf)) = Cons(f x, fn () => seqMap f (xf()));

fun inter Nil yf = yf
  | inter (Cons(x, xf)) yf =
    Cons(x, fn () => inter yf (xf()));

fun seqGet 0 _ = []
  | seqGet _ Nil = []
  | seqGet n (Cons(x, xf)) = x::seqGet (n-1) (xf());

fun seqConcat Nil = Nil
  | seqConcat (Cons(Nil,xf)) = seqConcat (xf())
  | seqConcat (Cons(Cons(y,yf),xf)) =
  Cons(y, fn () => inter (yf()) (seqConcat (xf())));

fun seqCross Nil yf = Nil
  | seqCross xf Nil = Nil
  | seqCross (Cons(x,xf)) (Y as Cons(y,yf)) =
  Cons((x,y), fn () => inter (seqMap (fn z => (x,z)) (yf()))
                             (seqCross (xf()) Y));

(* Binary Trees with empty leaves *)
datatype 'a tree_lf = Lf | Br of 'a * 'a tree_lf * 'a tree_lf;
exception Lf_Node;

(* Can probably generalise this to compare key-value pairs *)
fun trUpdate Lf x = Br (x, Lf, Lf)
  | trUpdate (Br(node, left, right)) new =
  if node < new then Br(node, trUpdate left new, right)
  else if node > new then Br(node, left, trUpdate right new)
  else (* node = new *) Br(new, left, right);

fun trUnion t Lf = t
  | trUnion t1 (Br(node, left, right)) =
  trUnion (trUpdate t1 node) (trUnion left right);

(* Balanced deletion comes later *)

fun trInord t =
  let fun inord Lf vs = vs
        | inord (Br(v, left, right)) vs = 
        inord left (v::inord right vs)
  in inord t [] end;

fun trPreord t =
  let fun preord Lf vs = vs
        | preord (Br(v, left, right)) vs = 
        v::preord left (preord right vs)
  in preord t [] end;

fun trPostord t =
  let fun postord Lf vs = vs
        | postord (Br(v, left, right)) vs = 
        postord left (postord right (v::vs))
  in postord t [] end;

fun treeList [] = Lf
  | treeList xs = foldl (fn (x,e) => trUpdate e x) Lf xs;

(* Binary Trees with value leaves, get rid of this *)
datatype 'a tree =  Tw of 'a | Bv of 'a * 'a tree * 'a tree;

(*Bad hack *)
fun twUpdate (Tw node) new = 
  if node < new then Bv (new, Tw node, Tw new)
  else Bv (node, Tw new, Tw node)
  | twUpdate (Bv(node, left, right)) new =
  if node < new then Bv(node, twUpdate left new, right)
  else if node > new then Bv(node, left, twUpdate right new)
  else (* node = new *) Bv(new, left, right);

fun twUnion t (Tw node) = twUpdate t node
  | twUnion t1 (Bv(node, left, right)) =
  twUnion (twUpdate t1 node) (twUnion left right);

(* Balanced deletion comes later *)

fun twInord t =
  let fun inord (Tw v) vs = v::vs
        | inord (Bv(v, left, right)) vs = 
        inord left (v::inord right vs)
  in inord t [] end;

fun twPreord t =
  let fun preord (Tw v) vs = v::vs
        | preord (Bv(v, left, right)) vs = 
        v::preord left (preord right vs)
  in preord t [] end;

fun twPostord t =
  let fun postord (Tw v) vs = v::vs
        | postord (Bv(v, left, right)) vs = 
        postord left (postord right (v::vs))
  in postord t [] end;

fun twList (x::xs) = foldl (fn (x,e) => twUpdate e x) (Tw x) xs;
val it2 = Bv 
