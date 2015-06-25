datatype 'a queue = Q of ('a list * 'a list);
datatype 'a tree = Lf | Br of ('a * 'a tree * 'a tree);
fun norm (Q([],tl)) = Q(rev tl, []) | norm q = q;
fun qnull (Q([],[])) = true | qnull _ = false;
fun enq (Q(x,y), a) = norm(Q(a::x, y));
fun deq (Q(x::xs, y)) = norm(Q(xs,y));
fun qhd (Q(x::xs, y)) = x;
fun breadth q = 
    if qnull q then []
    else case qhd q of
                  Lf => breadth (deq q)
                | Br(v,t,u) => v::breadth(enq(enq(deq q, t), u));
val test = Q([Br(4, Br(2, Br(1, Lf, Lf), Br(3, Lf, Lf)), Br(6, Br(5, Lf, Lf), Br(7, Lf, Lf)))], []);
val flat = breadth test;
