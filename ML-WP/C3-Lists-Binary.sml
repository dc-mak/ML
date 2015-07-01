(* 3.15 *)
fun bincarry (false, ps) = ps
  | bincarry (true, []) = [true]
  | bincarry (true, p::ps) = not p :: bincarry (p, ps);


fun binsum (c, [], qs) = bincarry (c, qs)
  | binsum (c, ps, []) = bincarry (c, ps)
  | binsum (c, p::ps, q::qs) =
  let
    (* Paulson: At least two bits are true *)
    fun carry (a,b,c) = a andalso b orelse a andalso c orelse b andalso c;
    (* Paulson: b=c is equivalent to not (b XOR c) so... *)
    fun sum   (a,b,c) = (a=(b=c));

    (* What I did...
    fun xor (a, b) = a andalso not b orelse not a andalso b
    fun sum (a, b, c) = xor(xor(a,b),c)
    fun carry (a, b, c) = c andalso (a orelse b) orelse a andalso b *)
  in
    sum (p, q, c) :: binsum (carry (p, q, c), ps, qs)
  end;

fun binprod ([], _) = []
  | binprod (false::ps, qs) = false::binprod (ps, qs)
  | binprod (true::ps, qs) = binsum (false, qs, false::binprod (ps, qs));

(* 3.16: See the structure... All from Online solution's web answers. *)
signature ARITH =
    sig
    type t
    val zero : t
    val sum  : t * t -> t
    val diff : t * t -> t
    val prod : t * t -> t
    val quo  : t * t -> t
    end;

(* 3.17: TODO Redo 1995.1.6 *)
structure Bin : ARITH =
struct
  type  t = int list

  val zero = []

  (* Equivalent to add 1 *)
  fun carry (0, ps) = ps
    | carry (1, []) = [1]
    | carry (1, p::ps) = (1-p) :: carry (p, ps);

  (* Sum logic and carry logic propogated through list *)
  fun sumc (c, [], qs) = carry (c,qs)
    | sumc (c, ps, []) = carry (c,ps)
    | sumc (c, p::ps, q::qs) =
        ((c+p+q) mod 2)::sumc((c+p+q) div 2, ps, qs);

  (* Start with 0 carry *)
  fun sum (ps,qs) = sumc (0,ps,qs);

  (* Skip for 0, double for 1 and add recursively *)
  fun prod ([], _) = []
    | prod (0::ps, qs) = 0::prod(ps,qs)
    | prod (1::ps, qs) = sum(qs, 0::prod(ps,qs));

  (* 3.16: subtraction - Online solution *)
  infix $$;
  (* This is beautiful. Think of it like a special cons. *)
  fun 0 $$ [] = []          (* Gets rid of any leading 0s *)
    | n $$ [~1] = [~1]      (* Propogate/signal that result *)
    | n $$ ns = n::ns;      (* is negative. Otherwise, cons *)

  (* Equivalent to subtract one *)
  fun borrow (0, ps) = ps           (* Nothing to borrow *)
    | borrow (~1, []) = [~1]        (* Result is negative *)
    (* Flip bit: 1-1=0, 0-1=10-1=1 and subtract 1 from next *)
    | borrow (~1, p::ps) = (1-p) $$ borrow (p-1, ps);

  (* Invariants: b = 0 or ~1.
   *        b+p-q = 1, 0, -1, -2
   *        mod 2 = 1, 0,  1,  0
   *        div 2 = 0, 0, -1, -1 *)
  fun diffb (b, ps, []) = borrow (b,ps)
    (* To catch leading 0s, I think *)
    | diffb (b, [], q::qs) =
        ((b-q) mod 2) $$ diffb ((b-q) div 2, [], qs)
    | diffb (b, p::ps, q::qs) =
        ((b+p-q) mod 2) $$ diffb ((b+p-q) div 2, ps, qs);
    (* Standard bit-by-bit subtraction with borrow value. Write out
     * "truth" table of (b_in, x, y) -> (b_out, sum) to check:
     *          b_in    x   -y  |   s   b_out
     *          ----------------+------------
     *           0      0    0  |   0    0
     *           0      0   -1  |   1   -1
     *           0      1    0  |   1    0
     *           0      1   -1  |   1    0
     *          -1      0    0  |   0    0
     *          -1      0    0  |   0   -1
     *          -1      0   -1  |   0   -1
     *          -1      1    0  |   0    0
     *          -1      1   -1  |   0   -1
     *  to see that s = (b+p-q) mod 2 and b_out = (b+p-q) div 2. *)

  (* Start with 0 borrow *)
  fun diff (ps,qs) = diffb (0,ps,qs);

  (* Division: repeated subtraction... *)
  fun divide (divnd,dvsr,n,quos) =
    if n=0 then (quos,divnd)
    else
      let val rems = diff (divnd,dvsr)
      (* Just like long division: if dividend is too small,
       * add 0 to both the quotient and the dividend and try again. *)
      in  if  rems = [~1] then divide(0::divnd, dvsr, n-1, 0::quos)
      (* Because it's binary, we don't have to 'guess' the digit, it only
       * depends on whether or not subtraction works. *)
                          else divide(0::rems, dvsr, n-1, 1::quos)
      end;

  (*Scan down list counting bits in k; get position of last "1" (in n). *)
  fun lastone (n,k,[]) = n
    | lastone (n,k,0::ps) = lastone(n,k+1,ps)
    | lastone (n,k,1::ps) = lastone(k,k+1,ps);

  (* I was somewhat in the ballpark with original 1995.1.6, 2's complement
   * attempt. *)
  fun addzeros (0,ds) = ds
    | addzeros (k,ds) = 0::addzeros(k-1,ds);

  fun quorem (ps,ds) =
    let val n = lastone(0,1,ps) - lastone(0,1,ds)
    in if n<0 then ([0],ps)        (* n_ds > n_ps => ds > ps *)
      (* Add zeros for first difference calculation. *)
      else
        let val (qs,rs) = divide(ps, addzeros(n,ds), n+1, [])
        in if length rs < n+1 then (qs,rs)
           (* n to account for addzeros, +1 for last call of divide. *)
           else (qs, List.drop(rs,n+1))
        end
    end;

  fun quo (ps,qs) = #1(quorem(ps,qs))
  and rem (ps,qs) = #2(quorem(ps,qs));

  fun gcd(ms,ns) =
      if lastone(0,1,ms)=0 then  ns  else  gcd(rem(ns,ms), ms);
end;

(* Testing functions *)
fun toBin n =
let fun tB (0, res) = rev res : Bin.t
      | tB (n, res) = tB (n div 2, (n mod 2)::res)
in tB (n, []) end;

fun toInt [] = 0
  | toInt (y::ys) = y + 2*(toInt (ys : Bin.t));

(* 3.18: Looked at starting point. *)
val ten = toBin 10;

fun toBin_from_dec [] = []
  | toBin_from_dec (x::xs) =
    Bin.sum(toBin x, Bin.prod(ten, toBin_from_dec xs));

(* Online solution (mine was totally on the wrong track). *)
fun toDec_from_bin [] = []
  | toDec_from_bin (x::xs) =
  let
    fun double (0, []) = []
      | double (c, []) = [c]
      | double (c, d::ds) =
      let val next = c + 2*d        (* Compute newest digit *)
      in (next mod 10)::double(next div 10, ds) end
  in           (* and recursively update the previous ones. *)
    double(x, toDec_from_bin (xs : Bin.t))
  end;

(* Simple enough once previous figured out *)
fun fact_bin 0 = [1]
  | fact_bin n = Bin.prod(toBin n, fact_bin (n-1));
rev (toDec_from_bin (fact_bin 100));
