datatype seq = sNil | node of int * (unit -> seq);
datatype seqpair = pNil | pair of (int * int) * (unit -> seqpair);

fun quad n = node ((4-n)*(n+4), fn () => 
             if ((4-n)*(n+4) <= 0) then sNil else quad (n+1));

fun nat n = node(n, fn () => 
             if (n > 9) then sNil else nat(n+1));

fun add (n, sNil) =  pNil
  | add(n, node(y, yf)) = pair((n, y), fn () => add(n, yf()));

fun interleave (pNil, bf) = bf
  | interleave (pair((a1, a2), af), bf) = 
  pair((a1, a2), fn () => interleave(bf, (af())));

fun pairup (sNil, _) = pNil
  | pairup (_, sNil) = pNil
  | pairup (node(n, nf), node(y, yf)) = 
  pair((n, y), fn () => interleave((add(n, yf())), pairup(nf(), node(y, yf))));

fun next sNil = sNil
  | next (node(n, nf)) = nf();

fun get (_, pNil) = []
  | get (0, pair((a1,a2), af)) = []
  | get (n, pair((a1,a2), af)) = (a1, a2)::get(n-1, af());

val p = ref 0;
fun prnt [] = (print "|-|\n"; [])
  | prnt ((a1,a2)::b) = (print ("("^Int.toString(a1)^","^Int.toString(a2)^")");
                         p := !p + 1; if (!p mod 10 = 0) then print "\n" else ();
                         prnt(b)); 
  
val a = nat 1;
val b = quad ~3;
val pairlist = get(400, pairup(a,a));
prnt pairlist;
val pairlist = get(400, pairup(a,b));
prnt pairlist;
