(* 6.1:
 * Assume n and d are natural numbers such that d <> 0.
 * Base case, n = 0. We choose q = 0 and r = 0 and to satisfy the conclusion.
 * Inductive hypothesis - assume there exists q, r such that k = dq + r.
 * For k+1, there are two possibilities, either 1 <= r+1 < d or r+1 = d.
 * In the first case, let r' = r+1 and in the second, let r' = 0 and q' = q+1. *)

(* O(n) time. Code corrected to reflect proof more accurately. *)
fun div_alg (n, r) =
     if  n = 0 then (0, 0)
     else let val (q, r') = div_alg (n-1, r) in
       if r' < r then (q, r'+1) else (q+1, 0) end;

use "working-programmer/examples/sample3-sets.sml";
fun check_div_alg () =
  let
    fun check ((x,y),e) =
        ((x div y, Int.rem(x,y)) = div_alg (x,y)) andalso e
    val xs = List.tabulate (20, fn i => i+1)
  in
    foldl check true (cartprod (xs,xs))
  end;

(* 6.2:
 * Assume P(n) can be proven by mathematical induction.
 * That means we have proven P(0) and P(n) => P(n+1) for all n.
 * For complete induction, we show P(n) by assuming P(k) for all k < n.
 * If n = 0, then we are done by assumption of P(0) being proven.
 * If n = k+1, then we can use P(k) by assumption.  *)

(* 6.3:
 * Assume P(n) can be proven by complete induction. Then we can use the
 * induction formula P'(n) = for all k < n, P(k) to modify the inductive
 * hypothesis. Now we induct on n in P'(n) mathematically.
 * P'(0) <=> for all k < 0, P(k). Second clause is vacuously true.
 * P'(k) => P'(k+1) is done by assuming P(x) for all x < k. To show P'(k+1) is
 * to show P(x) for all x < k+1. This means P(x) for all x < k or k=x.
 * The former holds by the inductive hypothesis and the latter by the fact we
 * have proven P by complete induction already. *)

(* 6.4: Corrected to use inequalities in 2.16 for a more concise proof.
 * fun increase (k, n) = if n < (k+1)*(k+1) then k else k+1;
 *
 * fun introot n = if n=0 then 0 else increase(2*introot(n div 4), n);
 *
 * Since the recursive call involves n div 4, we use complete induction.
 * The case for 0 is trivial.
 *
 * Assume for all 0<k<n, introot k = is correct.
 * Then for introot n where n = 4m+r for r=0,1,2,3 (so 4m <= n)
 *     introot m = i such that i^2 <= m < (i+1)^2 and so
 *     4i^2 <= 4m <= n < 4m+4 < 5(i+1)^2 meaning,
 *     (2i)^2 <= n < (2i+1)^2 are the limits to test for adding 1. *)

(* 6.5: Floating point arithmetic isn't exact, the formula would need a
 * complicated proof by complete induction as well as the fact that termination
 * is not a straightforward base case.
 * Additionally (online solutions) the method is based on calculus and so is a
 * little tougher to formalise. *)

(* 6.6: Prove xs @ [] = xs for all lists xs.
 *      Base case, xs = []. Therefore, by definition of @, [] @ [] = [].
 *      Inductive hypothesis is the same as the proof statement.
 *      We show (x::xs) @ [] = x::xs. By definition of @, we have x::(xs@[]).
 *      By the inductive hypothesis, we have x::(xs@[]) = x::xs as required. *)

(* 6.7: Prove l1 @ (l2 @ l3) = (l1 @ l2) @ l3.
 *      Induct on l1. Base case is l1 = []. Hence, both sides are equal as
 *      required. We use the proof statement as the induction hypothesis and
 *      seek to prove that (l::l1) @ (l2 @ l3) = (l::l1 @ l2) @ ls.
 *      l :: (l1 @ (l2 @ l3)) = l :: (l1 @ l2) @ l3         [ind hyp]
 *                            = ((l::l1) @ l2) @ l3.  *)

(* 6.8: Prove nrev (nrev xs) = xs.
 *      Base case, xs = []. nrev (nrev []) = nrev [] = [].
 *      We use the proof statement as the inductive hypothesis and then seek to
 *      prove that nrev (nrev (x::xs)) = x::xs.
 *      nrev (nrev (xs) @ [x])
 *      nrev [x] @ nrev (nrev xs) by Theorem 10 (p228)
 *      [x] @ xs by Lemma in 6.6 and inductive hypothesis
 *      x::xs    by definition of @. *)

(* 6.9: Using the function definitions below, we wish to prove
 *      For all lists xs, nlength xs = length xs.
 *
 *      Base case: xs = [].
 *      length  []    = addlen (0, []) = 0
 *      nlength []    = 0 which are equal.
 *
 *      We take the proof statement as the inductive hypothesis.
 *      We seek to prove that length (x::xs) = nlength (x::xs).
 *
 *      nlength (x::xs) = 1 + nlength xs.
 *      length  (x::xs) = addlen (0, x::xs)
 *                      = addlen (1, xs)
 *                      = 1 + addlen (0, xs)xs  [see lemma below]
 *                      = 1 + nlength xs                [ind hyp].
 *
 *      Lemma: For all lists xs, addlen (n, xs) = n + addlen (0, xs).
 *      Base case:  xs = [], so
 *      addlen (p, []) = p + addlen (0, []) = p+0 = p.
 *
 *      Take lemma statement as ind. hyp. We seek to prove,
 *      addlen (n, x::xs) = n + 1 + addlen (0, xs).
 *      addlen (n, x::xs) = addlen (n+1, xs)
 *                        = n + 1 + addlen (0, xs)      [ind hyp]
 *      and so we are done. *)

(* 6.10: No binary tree equals its own left-subtree.
 *       Statement given in question is vacuously true for Lf since it has no
 *       left subtree.
 *
 *       So for non-Lf trees, we say for all t = Br(x,tl,tr), t <> tl.
 *       Base case: tl = Lf, and so t = Br(x, Lf, tr) <> Lf.
 *       Take the proof statement as the inductive hypothesis.
 *       Assume that for tl = Br(y, tll, tlr), tl <> tll and
 *                       tr = Br(z, trl, trr), tr <> trl.
 *
 *       Then for tree t = Br(x, Br(y, tll, tlr), Br(z, trl, trr)) we see that
 *                    t <> Br(y, tll, tlr). *)

(* 6.11: Prove size (reflect t) = size t.
 *       Base case: t = Lf, and so reflect Lf = Lf meaning
 *             size (reflect Lf) = size Lf = 0.
 *       Assume the proof statement as the inductive hypothesis.
 *       So size (reflect (Br(x, xl, xr))) =
 *           = size (Br(x, reflect xr, reflect xl))
 *           = 1 + size (reflect xr) + size (reflect xl)
 *           = 1 + size xr + size xl                               [ind hyp]
 *           = 1 + size xl + size xr                                [assoc.]
 *           = size (Br(x, xl, xr)) *)

(* 6.12: For all binary trees t, nlength (preorder t) = size t.
 *       Base case: t = Lf.
 *          size Lf = 0 = nlength (preorder Lf) = nlength [].
 *
 *       Assume proof statement for inductive hypothesis.
 *       size (Br(x,xl,xr)) = 1 + size xl + size xr
 *       nlength (preorder (Br(x,xl,xr))
 *              = nlength ([x] @ xl @ xr)
 *              = nlength [x] + nlength xl + nlength xr         [Theorem 8]
 *              = 1 + size xl + size xr                           [ind hyp] *)

(* 6.13: For all binary trees t, nrev (inorder (reflect t)) = inorder t.
 *       Base case, t = Lf.
 *          inorder Lf = [] = nrev (inorder (reflect Lf)
 *                          = nrev (inorder Lf)
 *                          = nrev [].
 *
 *      Assume proof statement as inductive hypothesis.
 *      inorder (Br(x,xl,xr)) = inorder xl @ [x] @ inorder xr
 *      nrev (inorder (reflect (Br(x,xl,xr)))) =
 *      = nrev (inorder (Br(x, reflect xr, reflect xl)))
 *      = nrev (inorder (reflect xr) @ [x] @ inorder (reflect xl))
 *      = inorder (reflect xl) @ [x] inorder (reflect xr)
 *      = (nrev (inorder (reflect xl))) @ [x] @ (nrev (inorder (reflect xr)))
 *      = inorder xl @ [x] @ inorder xr                         [ind hyp] *)

(* 6.14: *)
datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

fun leaves Lf          = 1
  | leaves (Br(_,l,r)) = leaves l + leaves r;

(* Proof: Base case, leaves Lf = 1 = size Lf + 1 = 0 + 1.
 *        Assume that leaves t = size t + 1.
 *        So for leaves (Br(x,xl,xr)) = leaves xl + leaves xr and
 *                 size (Br(x,xl,xr)) = size xl + size xr + 1
 *        Now we apply the inductive hypothesis:
 *          size (Br(x,xl,xr)) = (leaves xl - 1) + (leaves xr - 1) + 1
 *                             = leaves xl + leaves xr - 1
 *        to see that leaves (Br(x,xl,xr)) = size (Br(x,xl,xr)) + 1. *)

(* 6.15: Prove: For all trees t, preord (t, []) = preorder t.
 *       Base case: t = Lf, preord (Lf, []) = [] = preorder Lf.
 *       Assume proof statement for inductive hypothesis.
 *       preord (Br(x,xl,xr), [])
 *         = x::preord (xl, preord (xr, []))
 *         = x :: preord (xl, []) @ preord (xr, [])              [Lemma]
 *         = [x] @ preorder xl @ preorder xr.                  [ind hyp]
 *
 *       preorder (Br(x,xl,xr)) = [x] @ preorder xl @ preorder xr
 *
 *       Lemma: preord (t, vs) = preord (t, []) @ vs.
 *       Base case, preorder (Lf, vs) = vs.
 *       Assume lemma statement for inductive hypothesis.
 *       preord (Br(x,xl,xr), x::preord (xl, preord (xr, []))
 *         = x::preord (xl, []) @ preorder (xr, []). *)

(* 6.16: Prove map f (xs @ ys) = (map f xs) @ (map f ys)
 *       Base case, xs = [], map f ([] @ ys) = map f ys and
 *       (map f []) @ (map f ys) = [] @ (map f ys) = map f ys.
 *
 *       Assume proof statement as inductive hypothesis.
 *       We show that map f (x::xs @ ys) = (map f (x::xs)) @ (map f ys).
 *       map f (x::xs @ ys) = (f x) :: map f (xs @ ys).
 *                          = (f x) :: (map f xs) @ (map f ys)  [ind hyp]
 *                          = (map f (x::xs)) @ (map f ys)          [map] *)

(* 6.17: Prove (map f) o nrev = nrev o (map f).
 *       For all lists xs, ((map f) o nrev) xs = (nrev o (map f)) xs.
 *       Base case: xs = []. (map f (nrev [)]) = map f [] = [].     [comp]
 *                           (nrev (map f [])) = nrev  [] = [].     [comp]
 *       Assume quantified proof statement as inductive hypothesis.
 *       We show map f (nrev (x::xs)) = nrev (map f (x::xs)).
 *       map f (nrev xs @ [x]) = (map f (nrev xs)) @ (map f [x])    [6.16]
 *                             = nrev (map f xs) @ [f x]
 *                             = nrev ((f x) :: map f xs))
 *                             = nrev (map f (x::xs)).              [map] *)

(* 6.18: *)
fun maptree f Lf            = Lf
  | maptree f (Br(x,xl,xr)) = Br(f x, maptree f xl, maptree f xr);

(* Prove: ((maptree f) o reflect) = (reflect o (maptree f)).
 *        For all trees t, maptree f (reflect t) = reflect (maptree f t).
 *        Base case, t = Lf. By definition of composition:
 *        maptree f (reflect Lf) = maptree f Lf = Lf.
 *        reflect (maptree f Lf) = reflect Lf   = Lf.
 *        Assume proof statement as inductive hypothesis for t = xl,xr.
 *        We show ((maptree f) o reflect) (Br(x,xl,xr))
 *                  = reflect (maptree f (Br(x,xl,xr))).
 *
 *        maptree f (reflect (Br(x,xl,xr)))
 *          = maptree f (Br(x, reflect xr, reflect xl))
 *          = Br(f x, maptree f (reflect xr), maptree f (reflect xl))
 *          = Br(f x, reflect (maptree f xr), reflect (maptree f xl))
 *          = reflect (Br(x, maptree f xl, maptree f xr))
 *          = reflect (maptree f (Br(x,xl,xr))).
 *
 * Prove: (map f) o preorder = preorder o (maptree f).
 *        For all trees t, map f (preorder t) = preorder (maptree f t).
 *        Base case, t = Lf, map f (preorder Lf) = map f [] = [].
 *                    preorder (maptree f Lf) = preorder Lf = [].
 *
 *        Assume second proof statement as inductive hypothesis for t=xl,xr.
 *        We show that
 *        map f (preorder (Br(x,xl,xr))) = preorder (maptree f (Br(x,xl,xr))).
 *        map f (preorder (Br(x,xl,xr)))
 *          = map f ([x] @ preorder xl @ preorder xr)
 *          = (map f [x]) @ (map f (preorder xl)) @ (map f (preorder xr))
 *          = [f x] @ (preorder (maptree f xl)) @ (preorder (maptree f xr))
 *          = preorder (Br(f x, maptree f xl, maptree f xr))
 *          = preorder (maptree f (Br(x,xl,xr))). *)

(* 6.20: Prove foldr (op::) ys xs = xs @ ys.
 *       Base case, xs = []. foldr (op::) ys [] = ys = [] @ ys = ys.
 *       Assume proof statement as inductive hypothesis.
 *       We show foldr (op::) ys (x::xs) = (x::xs) @ ys.
 *       op:: (x, foldr (op::) ys xs)
 *          = op:: (x, xs @ ys) = (x::xs) @ ys. *)

(* 6.21: Let foldr (op+) = F. Show that for all l,y, (F e l)+y  = F y l.
 *       Base case: Let l = []. (F e l)+y = e+y = y = F y [].
 *       Let the proof statement be the inductive hypothesis for l=xs.
 *       Let l = x::xs.
 *       (F e (x::xs))+y = x+(F e xs)+y = x + (F y xs) = F y (x::xs). *)

(* 6.22: Let G(l,z) = F z l = foldr (op+) z l.
 *       Prove that for all ls, foldr G e ls = F e (map (F e) ls).
 *       Base case: ls = [].
 *       foldr G e [] = e = F e (map (F e) []) = F e [] = e.
 *
 *       Assume proof statement as inductive hypothesis.
 *       We show that foldr G e (l::ls) = F e (map (F e) (l::ls)).
 *       foldr G e (l::ls)
 *          = G(l, foldr G e ls)
 *          = G(l, F e (map (F e) ls))
 *          = F (F e (map (F e) ls)) l
 *          = op+(F e l, F e (map (F e) ls))                    [6.21]
 *          = F e ((F e l)::map (F e) ls)
 *          = F e (map (F e) (l::ls)). *)

(* 6.23: P(Atoms) & (P(prop) => P(Neg prop))
 *                & (P(p1) ^ P(p2) => P(Disj(p1,p2)) & P(Conj(p1,p2))).
 *
 *       Base case: Atoms.
 *          1. nnf (Atom a) = Atom a = nnfpos (Atom a).
 *          2. nnf (Neg (Atom a)) = Neg (Atom a) = nnfneg (Atom a)
 *
 *       Assume nnf p  = nnfpos p  & nnf (Neg p)  = nnfneg p, for all p/q.
 *       Show   nnf p' = nnfpos p' & nnf (Neg p') = nnfneg p'.
 *
 *       p' = Neg p
 *          1. nnf (Neg p)
 *              = nnfneg p                                      [ind hyp]
 *              = nnfpos (Neg p)
 *          2. nnf (Neg (Neg p))
 *              = nnf p
 *              = nnfpos p                                      [ind hyp]
 *              = nnfneg (Neg p)
 *
 *       p' = Conj(p,q).
 *          1. nnf (Conj (p,q))
 *              = Conj (nnf p, nnf q)
 *              = Conj (nnfpos p, nnfpos q)                     [ind hyp]
 *              = nnfpos (Conj (p,q))
 *          2. nnf (Neg (Conj(p,q))
 *              = nnf (Disj (Neg p, Neg q))
 *              = Disj (nnf (Neg p), nnf (Neg q))
 *              = Disj (nnfneg p, nnfneg q)                     [ind hyp]
 *              = nnfneg (Conj (p,q))
 *
 *       p' = Disj(p,q)
 *          1. nnf (Disj (p,q))
 *              = Disj (nnf p, nnf q)
 *              = Disj (nnfpos p, nnfpos q)                      [ind hyp]
 *              = nnfpos (Disj (p,q))
 *          2. nnf (Neg (Disj (p,q))
 *              = nnf (Conj (Neg p, Neg q))
 *              = Conj (nnf (Neg p), nnf (Neg q))
 *              = Conj (nnfneg p, nnfneg q)                      [ind hyp]
 *              = nnfneg (Disj(p,q))
 *
 *       And so we have proven the given statement. *)

(* 6.24:                                      [Isnnf(p), Isnnf(q)]
 *       Issnnf(Atom p)  Isnnf(Neg (Atom a))  Conj(p,q)  Disj(p,q)
 *       --------------------------------------------------------
 *                          Issnnf(r)                               *)

 datatype prop = Atom of string
               | Neg  of prop
               | Conj of prop * prop
               | Disj of prop * prop;

fun is_nnf (Atom a)       = true
  | is_nnf (Neg (Atom a)) = true
  | is_nnf (Neg (Neg p))  = false
  | is_nnf (Neg p)        = is_nnf p
  | is_nnf (Conj(p,q))    = is_nnf p andalso is_nnf q
  | is_nnf (Disj (p,q))   = is_nnf p andalso is_nnf q;

(* Proof: Isnnf (nnf (Atom a))       = Isnnf (Atom a)       = true.
 *        Isnnf (nnf (Neg (Atom a))) = Isnnf (Neg (Atom a)) = true.
 *
 *        Assume Issnnf(nnf p)    = true and Isnnf(nnf q) = true.
 *        Show that Isnnf(nnf p') = true.
 *
 *        Case p' = Neg (Neg p).
 *        Issnnf (nnf (Neg (Neg p))) = Isnnf (nnf p) = true     [ind hyp]
 *        
 *        Case p' = Conj(p,q)
 *        Issnnf (nnf (Conj(p,q))) 
 *          = Issnnf (Conj(nnf p, nnf q))
 *          = Issnnf (nnf p) andalso Issnnf (nnf q)
 *          = true andalso true.                                [ind hyp]
 *
 *        Case p' = Disj(p,q)
 *        Issnnf (nnf (Disj(p,q))) 
 *          = Issnnf (Disj(nnf p, nnf q))
 *          = Issnnf (nnf p) andalso Issnnf (nnf q)
 *          = true andalso true.                                [ind hyp]
 *        
 *        Case p' = Neg p where p is a Conj or a Disj. (Other cases done)
 *        Isnnf (nnf (Neg (__j (p,q)))
 *          = Isnnf (---j (nnf (Neg p), nnf (Neg q)))
 *          = Issnnf (nnf (Neg p)) andalso (Issnnf (nnf (Neg q)))
 *          = true and true.                                    [ind hyp] *)

(* 6.25: Tr(nnf (Atom a))      = Tr(Atom a) by definition of nnf, and
 *       Tr(nnf (Neg (Atom a)) = Tr(Neg (Atom a)), similarly.
 *
 *       Assume Tr(nnf p) = Tr(p).
 *       Tr(nnf (Neg (Neg p)) = Tr(nnf p) = Tr(p)
 *       Tr(Neg (Neg p))      = ¬¬Tr(p)   = Tr(p)
 *
 *       Assume Tr(nnf p) = Tr(p) for all p/q *AND* Neg p/Neg q.
 *       Tr(nnf (Neg (Conj(p,q)))
 *          = Tr(Disj(nnf (Neg p), nnf (Neg q)))
 *          = Tr(nnf (Neg p) | Tr(nnf (Neg q))
 *          = Tr(Neg p) | Tr (Neg q)
 *          = ¬Tr(p) | ¬Tr(q)                                   [ind hyp]
 *          = ¬(Tr(p) & Tr(q))                                  [deMorg']
 *          = Tr(Neg (Conj(p,q)))
 *       and similarly for Disj.
 *
 *       Assume Tr(nnf p) = Tr(p) for all p/q.
 *       Tr(nnf (Conj(p,q))
 *          = Tr(Conj(nnf p, nnf q))
 *          = Tr(nnf p) & Tr(nnf q)
 *          = Tr(p) & Tr(q)                                     [ind hyp]
 *          = Tr(Conj(p,q)). *)

(* 6.26: A dictionary, where x is a natural number key, or any case where
 * termination is dependent only on the first argument (e.g. in facti) where
 * the second one is ignored or used as an accumulator. *)

(* 6.27: I shall use the relation defined thus:
 *          (m,n) <_P (m',n') iff m < m' or (m = m' and n + 1 = n').
 *       Base case: ack (0, n) = n+1.
 *
 *       Assume ack (m+1, n) is defined.
 *       Then, ack (m, 0) = ack (m-1, 1) is defined because the recursive
 *       call has no infinite decreasing chains.
 *       
 *       Assume ack(m, n)  is defined.
 *       Therefore ack(m+1,n) = ack (m, ack(m+1, n-1)) is defined because
 *              (m+1,n) <_P (m_1, n-1) and (m+1, n) < (m, ?).
 *
 *      Prove ack (m,n) > m+n.
 *      Base case ack (0,n) = n+1 > 0 + n.
 *      
 *      Assume ack (m, 0) > m.
 *      Then ack (m-1, 1) > m-1+1 > m.
 *
 *      Assume ack (m,n) > m+n.
 *      Then, ack (m+1,n) = ack (m-1, ack(m,n-1))
 *            ack (m, n-1) > m+n-1.
 *            ack (m-1, m+n-1) > m+n+1 > 2m+n-2. *)

(* 6.28: The predecessor relation, if x = pred (y) and y = pred (z) then
 *       x <> pred (z). The transitive closure is then the big union over all
 *       compositions over the predecessor and so in this case would be "<".
 *
 *       Assume pred is well-founded, we prove that "<", the transitive closure
 *       of pred is well-founded. If pred is well founded, then there are no
 *       infinite decreasing chains of pred. This means there must be a least
 *       element. This least element must all be part of the transitive closure,
 *       less than all elements. Therefore, "<" is well-founded too.

(* 6.29: Let the relation be n <_2 n' iff n = n'-2. The domain of this function
 *       is the even numbers. Since the function's recursive call adheres to the
 *       relation <_2. *)

(* 6.30: Let nlength be the measure function over lists, so that structural
 * induction is mapped to the "<" relation over natural numbers.
 *
 * Online solution: take the cases. There is no tail to the empty list, so it
 * must be considered on its own. For non-empty lists, taking the <_L relation
 * over is saying that if P(xs) where xs is the tail of a list, then we can
 * assume it as a predecessor to proving P(x::xs). *)

(* 6.31: Termination of take ('a list * int -> 'a list)
 *       Prove that take (xs, i) terminates for all lists xs and integers i.
 *       Base case, xs = [], first clause returns [], thus terminating.
 *       Assume take (xs, i-1) terminates. We prove take (x::xs, i) terminates.
 *
 *       Two cases: i < 0. Therefore 'if' evaluates to [].
 *                  i > 0. Therefore, x::take(xs, i-1) which, by inductive
 *                  hypothesis, terminates.
 *
 *       Termination of drop ('a list * int -> 'a list).
 *       Prove that drop (xs, i) terminates for all lists xs and integers i.
 *       Base case, xs = [], first clause returns [], thus terminating.
 *       Assume drop (xs, i-1) terminates. We prove take (x::xs, i) terminates.
 *
 *       Two cases: i < 0. Therefore 'if' evaluates to x::xs, thus terminating.
 *                  i > 0. Therefore, drop (xs, i-1) which, by inductive
 *                  hypothesis, terminates.
 *
 *       Termination of merge ('a list * 'a list -> 'a list).
 *       Base case, let xs = []. Then ys is returned immediately by the first
 *       clause, thereby terminating. Similarly for case ys = [].
 *
 *       Assume merge (xs, y::ys) terminates and merge (x::xs, ys) terminates.
 *       Either result of x<=y ensures an element is cons'd to the result of 
 *       computation that will terminate.
 *
 *       Termination of tmergesort ('a list -> 'a list).
 *       Base case, xs = [], first clause returns [], thus terminating.
 *       Base case, xs = [x], first clause returns [x], thus terminating.
 *
 *       Assume tmergesort (xs') terminates for all xs' such that
 *       nlength xs' < nlength xs.
 *       Since merge, take, drop and tmergesort terminate by assumption, this
 *       too terminates as a result.
 *
 *       Case > of ordered(merge(xs,ys)).
 *       We show ordered(y::merge(x::xs', ys')).
 *       By inductive hypothesis, ordered(ys') and ordered(x::xs') so we may
 *       assume, ordered(merge(x::xs', ys')). Let v be the head of the returned
 *       list.
 *          Case ys' = []. In that case, merge (x::xs', []) = x::xs' so x > y
 *          or y < x as assumed and order is preserved.
 *          Case ys' = v::vs then
 *              Case v <= x so by assumption of ordered (y::v::vs), y <= v
 *              and so merge (x::xs', ys') = v::merge(xs, vs)
 *              Case v > x so by assumption of y < x, we have
 *              merge(x::xs', ys') = x::merge(xs',ys').         [ind hyp] *)

(* 6.32: It's a troll. Online solution - ordered'(l) = (tmergesort (l) = l).
 *       (->) By definition of Theorem 21,
 *            ordered'(l) -> (tmergesort(l) -> ordered(l)).
 *       (<-) Induction in the style of Theorem 24 apparently. *)

(* 6.33: (b1 + b2) (x) = b1(x) + b2(x) = b2 (x) + b1(x) = (b2 + b1)(x),
 *       by counting occurences and stating they should equal each other.
 *
 *       ((b1+b2)+b3)(x) = b1(x) + b2(x) + b3(x) = (b1+(b2+b3))(x),
 *       same as above. *)

(* 6.34: Prove bag(ins(x,xs)) = <x> + bag xs.
 *       Base case, xs = [], ins (x,[]) = [x], so bag [x] = <x>.
 *       Similarly, <x> + bag [] = <x> + 0  = <x>.
 *
 *       Assume bag(ins(x,ys)) = <x> + bag ys.
 *       Show bag(ins(x,y::ys)) = <x> + bag (y::ys).
 *       Case x <= y, then bag (x::y::ys) = <x> + bag (y::ys) by definition.
 *       Case x > y, the bag (y::ins(x,ys)) = <y> + bag (ins(x,ys)) 
 *                          = <y> + <x> + bag ys = <x> + bag (y::ys). 
 *       
 *       Prove bag(insort(xs)) = bag xs.
 *       Base case, xs = [], insort [] = [] so bag [] = bag [] = 0.
 *       Assume bag(insort(xs)) = bag (xs) = bag xs.
 *       We prove bag(insort(x::xs)) = bag (x::xs) = <x> + bag xs   [def.]
 *       bag (insort(x::xs))
 *          = bag (ins(x, insort xs)) 
 *          = <x> + bag (insort xs)                                 [above]
 *          = <x> + bag xs by inductive hypothesis. *)

(* 6.35: Replace all :: operations with newmem defined as in Chapter 3.
 *       Replace bag theorems with set theorems and go from there.
 *       Must add extra theorems to ensure uniqueness of each element.
 *       
 *       Online solution: test for equality of x=y and then only put 1.
 *       Using the inductive hypothesis, the two halves will only have unique
 *       elements, so when merging the ordered, unique halves, only need to do
 *       the simple test.
 *
 *       My solution would have had more overhead in switching between merge,
 *       newmem and mem but the runtime would be the same by the same inductive
 *       hypothesis as above, I think. *)
