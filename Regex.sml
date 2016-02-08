(* Invariant: always one start state and one end state. *)
type nfa = {n : int,                     (* number of states *)
            s : int,                     (* start state *)
            t : (int * char * int) list, (* transitions for non-empty characters *)
            d : (int * int) list,        (* epsilon-transitions *)
            f : int};                    (* end state *)

datatype regex = Union  of regex * regex
               | Concat of regex * regex
               | Star   of regex
               | Char   of char option;

fun disjoint ({n,s,t,d,f}, m) = 
    {n = n, s = s+m, f = f+m, 
     d = map (fn (x,y)   => (x+m, y+m))    d,
     t = map (fn (x,a,y) => (x+m, a, y+m)) t}

(* construct : regex -> nfa *)

    (* one start state that is accepting *)
fun construct (Char NONE) = {n=1, s=1, t=[], d=[], f=1}

    (* one start, one accepting and one symbol c transition *)
  | construct (Char (SOME c)) = {n=2, s=1, t=[(1,c,0)], d=[], f=0}

  (* one new starting and accepting state, epsilon to bypass and return *)
  | construct (Star rgx) =
    let val {n,s,t,d,f}  = construct rgx
        val (n', s', f') = (n+1, n+1, n+1)
    in  {n=n', s=s', t=t, d=(s',s)::(f,s')::d, f=f'} end

  (* reshuffling and ensuring disjoint unions *)
  | construct (Concat (rx1, rx2)) =
    let val {n=n2, s=s2, t=t2, d=d2, f=f2} = construct rx2
        val {n=n1, s=s1, t=t1, d=d1, f=f1} = disjoint (construct rx1, n2)
    in  {n=n1+n2, s=s1, t=t1@t2, d = (f1,s2)::d1@d2, f=f2} end

  (* one new starting and accepting state EACH, with epsilons to join *)
  | construct (Union (rx1, rx2)) =
    let val {n=n2, s=s2, t=t2, d=d2, f=f2} = construct rx2
        val {n=n1, s=s1, t=t1, d=d1, f=f1} = disjoint (construct rx1, n2)
        val n' = n1 + n2 + 2
        val s' = n' and f' = n'-1
    in  {n=n', s=s', t=t1@t2, d=(s',s1)::(s',s2)::(f1,f')::(f2,f')::d1@d2, f=f'} end

(* Sets *)
fun member (x, []) = false | member (x, y::ys) = x=y orelse member (x, ys)

fun union ([], ys)    = ys
  | union (x::xs, ys) = x::List.filter (fn y => x <> y) (union (xs, ys))

fun set [] = [] | set (x::xs) = union ([x], set xs)

(* Epsilon-transitions *)
fun epsln (q1, ds) =
  foldr (fn ((q1', q2), ds) => if q1=q1' then q2::ds else ds) [] ds

(* Character-transition *)
fun trans a (q0, ts) =
  foldr (fn ((q0',a',q1), ts) => if q0=q0' andalso a=a' then q1::ts else ts) [] ts

(* Depth-first search *)
fun depthf nexts ([], graph, visited) = rev visited
  | depthf nexts (x::xs, graph, visited) =
      depthf nexts (xs, graph, if member (x, visited) then visited 
                        else depthf nexts (nexts (x, graph), graph, x::visited))

(* One step transitions over a set, surprisingly useful *)
fun onestep nexts (qs, graph) = foldr (fn (q,qs) => nexts (q, graph) @ qs) [] qs

(* Epsilon-closure *)
fun closure ds =
    let val q0s       = set (map #1 ds)
        fun nextqs q0 = List.filter (fn q => q<>q0) (depthf epsln ([q0], ds, []))
        fun close (q0, qs) = map (fn q1 => (q0, q1)) (nextqs q0) @ qs
    in  foldr close [] q0s end

(* epsilon then transitions combined with transitions then epsilon *)
fun nextqs a (q0,(t,d)) =
  let val trans_then_eps = onestep epsln (trans a (q0,t), d)
      and eps_then_trans = onestep (trans a) (epsln (q0, d), t)
  in set (trans_then_eps @ eps_then_trans) end

(* orelse to account for the state from epsilon->transition->epsilon *)
fun run (qs, f, _, d, []) =
      member (f, qs) orelse member (f, onestep epsln (qs, d))
  | run (qs, f, t, d, c::cs) =
      run (onestep (nextqs c) (qs,(t,d)), f, t, d, cs)

fun feed {n,s,t,d,f} cs = run ([s], f, t, closure d, cs)

fun matches regex str = feed (construct regex) (explode str)

val test = Concat(Star(Union(Char (SOME #"a"), Char (SOME #"b"))), Char (SOME #"b"))
val (automaton as {n,f,s,t,d}) = construct test;
val d' = closure d;
