(**** ML Programs from Chapter 3 of

  ML for the Working Programmer, 2nd edition
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1996)

Copyright (C) 1996 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.

Additional comments added by Dhruv Makwana.
****)

use "structures/ARITH.sig";

signature BINARY =
  sig
  include ARITH
  val fromInt : int -> t
  val toInt : t -> int
  end;

structure Bin : BINARY =
  struct
  type  t = int list

  val zero = []

  (* Equivalent to add 1 *)
  fun carry (0, ps)    = ps
    | carry (1, [])    = [1]
    | carry (1, p::ps) = (1-p) :: carry (p, ps);

  (* Sum logic and carry logic propogated through list *)
  fun sumc (c, [], qs)       = carry (c,qs)
    | sumc (c, ps, [])       = carry (c,ps)
    | sumc (c, p::ps, q::qs) =
        ((c+p+q) mod 2)::sumc((c+p+q) div 2, ps, qs);

  (* Start with 0 carry *)
  fun sum (ps,qs) = sumc (0,ps,qs);

  (* Skip for 0, double for 1 and add recursively *)
  fun prod ([], _)     = []
    | prod (0::ps, qs) = 0::prod(ps,qs)
    | prod (1::ps, qs) = sum(qs, 0::prod(ps,qs));

  (* 3.16: subtraction - Online solution *)
  infix $$;
  (* This is beautiful. Think of it like a special cons. *)
  fun 0 $$ []   = []          (* Gets rid of any leading 0s *)
    | n $$ [~1] = [~1]      (* Propogate/signal that result *)
    | n $$ ns   = n::ns;      (* is negative. Otherwise, cons. *)

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
  fun lastone (n,k,[])    = n
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

  (* Put in structure after reading Chapter 7. *)
  fun fromInt n =
      let fun tB (0, res) = rev res : t
            | tB (n, res) = tB (n div 2, (n mod 2)::res)
      in tB (n, []) end;

  fun toInt [] = 0
    | toInt (y::ys) = y + 2*(toInt (ys : t));
  end;
