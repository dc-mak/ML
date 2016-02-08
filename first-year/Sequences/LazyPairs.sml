datatype seq = node of int * (unit -> seq);
datatype seqpair = pair of (int * int) * (unit -> seqpair);

fun natural n = node (n, fn() => natural (n+1));
fun square n = node (n*n, fn () => square (n+1));

fun add (n, node(y, yf)) = pair ((n, y), fn () => add(n, yf()));

fun interleave (pair((a1, a2), af), bf) = 
  pair ((a1, a2), fn () => interleave (bf, (af())));

fun pairup (node(n, nf), node(y, yf)) = 
  pair ((n, y), fn () =>
                interleave ((add(n, yf())), pairup(nf(), node(y, yf))));

fun next (pair((a1, a2), af)) = af();
val a = natural 1;
val b = square 1;

val stella = pairup (a, b);

fun get (0, pair((a1, a2), af)) = []
  | get(n, pair((a1, a2), af)) = (a1, a2)::get(n-1, af());
