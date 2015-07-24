(* 3.15: *)
fun bincarry (false, ps)    = ps
  | bincarry (true,  [])    = [true]
  | bincarry (true,  p::ps) = not p :: bincarry (p, ps);


fun binsum (c, [],       qs) = bincarry (c, qs)
  | binsum (c, ps,       []) = bincarry (c, ps)
  | binsum (c, p::ps, q::qs) =
  let
    (* Online solution: At least two bits are true *)
    fun carry (a,b,c) = a andalso b orelse a andalso c orelse b andalso c;
    (* Online solution: b=c is equivalent to not (b XOR c) so... *)
    fun sum   (a,b,c) = (a=(b=c));

    (* What I did...
    fun xor (a, b) = a andalso not b orelse not a andalso b
    fun sum (a, b, c) = xor(xor(a,b),c)
    fun carry (a, b, c) = c andalso (a orelse b) orelse a andalso b *)
  in
    sum (p, q, c) :: binsum (carry (p, q, c), ps, qs)
  end;

fun binprod ([], _)         = []
  | binprod (false::ps, qs) = false::binprod (ps, qs)
  | binprod (true::ps, qs)  = binsum (false, qs, false::binprod (ps, qs));

(* 3.16: See the structure... All from Online solution's web answers. *)
(* 3.17: TODO Redo 1995.1.6 *)
use "working-programmer/examples/ARITH.sig";
use "working-programmer/examples/Bin.sml";

(* 3.18: Looked at starting point. *)
val ten = Bin.fromInt 10;

fun toBin_from_dec [] = []
  | toBin_from_dec (x::xs) =
    Bin.sum(Bin.fromInt x, Bin.prod(ten, toBin_from_dec xs));

(* Online solution (mine was totally on the wrong track). *)
fun toDec_from_bin [] = []
  | toDec_from_bin (x::xs) =
  let
    fun double (0, [])    = []
      | double (c, [])    = [c]
      | double (c, d::ds) =
      let val next = c + 2*d        (* Compute newest digit *)
      in (next mod 10)::double(next div 10, ds) end
  in           (* and recursively update the previous ones. *)
    double(x, toDec_from_bin (xs : Bin.t))
  end;

(* Simple enough once previous figured out *)
fun fact_bin 0 = [1]
  | fact_bin n = Bin.prod(Bin.fromInt n, fact_bin (n-1));
rev (toDec_from_bin (fact_bin 100));
