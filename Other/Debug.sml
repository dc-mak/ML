fun show x = (fn (x, ()) => x) (x, print (Int.toString x));

fun bind x = (x, ());

fun ret (x, ()) = x;

fun prnt (x, y) = (x, print (y^"\n"));

fun cnvReal (x, ()) = (x, Real.toString x);

fun cnvInt (x, ()) = (x, Int.toString x);

fun show toString pre post = ret o post o prnt o toString o pre o bind;

fun idShow toString = show toString (fn x => x) (fn x => x);

val showInt = idShow cnvInt;

val showReal = idShow cnvReal;

infix 9 <<>>;
fun f <<>> g = f g;
