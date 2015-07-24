signature BINARY =
  sig
  include ARITH
  val fromInt : int -> t
  val toInt   : t -> int
  val quorem  : t * t -> t * t
  val rem     : t * t -> t
  val gcd     : t * t -> t
  val fromDec : int list -> t
  val toDec   : t -> int list
  end;

(* This could be done with datatype bit = Z | O
 *                         datatype t   = B of bit list | Neg
 * but that would complicate the code/convenience of +,-, div and mod. *)
structure Bin :> BINARY where type t' = int list =
  struct
  type t' = int list
  datatype t  = B of t'

  val zero = B []

  fun check [] = true
    | check (x::xs) = (x=1 orelse x=0) andalso check xs
  
  exception Not_Binary
  fun make xs     = if check xs then B xs else raise Not_Binary
  fun dest (B xs) = xs

  (* Put in structure after reading Chapter 7. *)
  fun fI 0 = []
    | fI n = n mod 2 :: fI (n div 2)

  fun fromInt n = B (fI n)

  fun tI []      = 0
    | tI (x::xs) = x + 2 * tI xs

  fun toInt (B xs) = tI xs

  fun toString (B xs) =
    let fun toS ([], str)    = str
          | toS (y::ys, str) = toS (ys, str ^ Int.toString y)
    in  toS (xs, "") end

  fun neg (B xs) = B [~1]
  (* Equivalent to add 1 *)
  fun carry (0, ps)    = ps
    | carry (1, [])    = [1]
    | carry (1, p::ps) = (1-p) :: carry (p, ps)
    | carry _          = raise Not_Binary

  (* Sum logic and carry logic propogated through list *)
  fun sumc (c, [], qs)       = carry (c,qs)
    | sumc (c, ps, [])       = carry (c,ps)
    | sumc (c, p::ps, q::qs) =
        ((c+p+q) mod 2)::sumc((c+p+q) div 2, ps, qs)

  (* Start with 0 carry *)
  fun sum (B ps, B qs) = B (sumc (0,ps,qs))

  (* Skip for 0, double for 1 and add recursively *)
  fun prod' ([], _)     = []
    | prod' (0::ps, qs) = 0::prod' (ps,qs)
    | prod' (1::ps, qs) = sumc(0, qs, 0::prod' (ps,qs))
    | prod' _           = raise Not_Binary

  fun prod (B ps, B qs) = B (prod' (ps, qs))

  (* 3.16: subtraction - Online solution *)
  infix $$
  (* This is beautiful. Think of it like a special cons. *)
  fun 0 $$ []   = []          (* Gets rid of any leading 0s *)
    | n $$ [~1] = [~1]      (* Propogate/signal that result *)
    | n $$ ns   = n::ns     (* is negative. Otherwise, cons. *)

  (* Equivalent to subtract one *)
  fun borrow (0, ps)     = ps                (* Nothing to borrow *)
    | borrow (~1, [])    = [~1]             (* Result is negative *)
    (* Flip bit: 1-1 = 0, 0-1 = 10-1 = 1 and subtract 1 from next *)
    | borrow (~1, p::ps) = (1-p) $$ borrow (p-1, ps)
    | borrow _           = raise Not_Binary

  (* Invariants: b = 0 or ~1.
   *        b+p-q = 1, 0, -1, -2
   *        mod 2 = 1, 0,  1,  0
   *        div 2 = 0, 0, -1, -1 *)
  fun diffb (b, ps, []) = borrow (b,ps)
    (* To catch leading 0s, I think *)
    | diffb (b, [], q::qs) =
        ((b-q) mod 2) $$ diffb ((b-q) div 2, [], qs)
    | diffb (b, p::ps, q::qs) =
        ((b+p-q) mod 2) $$ diffb ((b+p-q) div 2, ps, qs)
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
  fun diff (B ps, B qs) = B (diffb (0,ps,qs))

  (* Division: repeated subtraction... *)
  fun divide (divnd,dvsr,n,quos) =
    if n=0 then (quos,divnd)
    else
      let val rems = diffb (0, divnd, dvsr)
      (* Just like long division: if dividend is too small,
       * add 0 to both the quotient and the dividend and try again. *)
      in  if  rems = [~1] then divide(0::divnd, dvsr, n-1, 0::quos)
      (* Because it's binary, we don't have to 'guess' the digit, it only
       * depends on whether or not subtraction works. *)
                          else divide(0::rems, dvsr, n-1, 1::quos)
      end

  (*Scan down list counting bits in k; get position of last "1" (in n). *)
  fun lastone (n,k,[])    = n
    | lastone (n,k,0::ps) = lastone(n,k+1,ps)
    | lastone (n,k,1::ps) = lastone(k,k+1,ps)
    | lastone _           = raise Not_Binary

  (* I was somewhat in the ballpark with original 1995.1.6, 2's complement
   * attempt. *)
  fun addzeros (0,ds) = ds
    | addzeros (k,ds) = 0::addzeros(k-1,ds)

  fun quorem (B ps, B ds) =
    let val n = lastone(0,1,ps) - lastone(0,1,ds)
    in if n<0 then (B [0], B ps)        (* n_ds > n_ps => ds > ps *)
      (* Add zeros for first difference calculation. *)
      else
        let val (qs,rs) = divide(ps, addzeros(n,ds), n+1, [])
        in if length rs < n+1 then (B qs, B rs)
           (* n to account for addzeros, +1 for last call of divide. *)
           else (B qs, B (List.drop(rs,n+1)))
        end
    end

  fun divd (ps,qs) = #1(quorem(ps,qs))
  and rem (ps,qs)  = #2(quorem(ps,qs))

  fun eq (B [], B [])          = true
    | eq (B (x::xs), B(y::ys)) = x=y andalso eq (B xs, B ys)
    | eq (_, _)                = false

  fun gcd(B ms, B ns) =
      if lastone(0,1,ms)=0 then  B ns else gcd(rem(B ns, B ms), B ms)

  (* And some fun ones *)
  val ten = fI 10

  fun fD [] = []
    | fD (d::ds) = sumc (0, fI d, prod' (ten, fD ds))

  fun fromDec xs = B (fD xs)

  fun double (0, [])    = []
    | double (c, [])    = [c]
    | double (c, d::ds) =
    let val next = c + 2*d
    in  (next mod 10):: double (next div 10, ds) end

  fun toDec (B []) = []
    | toDec (B (p::ps)) = double (p, toDec (B ps))
  end;
