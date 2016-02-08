(* Hey look, it's a non-uniform datatype! *)

datatype 'a seq = Nil 
                | Cons of unit -> 'a * 'a seq 
                | Nxt of 'a * 'a seq;
(* Any tuple of output and sequence is also a sequence
 * Allows us to define one next function for head and rest *)

fun infinite n = Cons (fn() => (n, infinite (n+1)));

exception Empty;

(* One next function to rule them all *)
fun next Nil               = raise Empty
  | next (Cons x)          = Nxt (#1(x()), #2(x()))
  | next (Nxt (_, Nil))    = raise Empty
  | next (Nxt (_, Cons x)) = Nxt (#1(x()), #2(x()))
  | next (Nxt (a, b))      = Nxt (a, b);
