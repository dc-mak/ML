(* This one is mainly for practice more than anything else *)
signature BINARY2 =
 sig
 include NUMBER
 val fromInt : int -> t
 val toInt   : t -> int
 val quorem  : t * t -> t * t
 val rem     : t * t -> t
 val gcd     : t * t -> t
 end;

(* http:/en.wikipedia.org/wiki/Two%27s_complement *)
structure Bin2 :> BINARY2 where type t' = int list =
  struct

  type t' = int list
  datatype Bit = O | I
  (* Could have datatype t = Bit * B of Bit list for (sign, two's comp)
   * so that sign won't take O(n) time unnecessarily *)
  type t = Bit list

  val zero = [O]
  val one  = [I, O]

  (* Preliminaries *)
  fun fromBool false = O
    | fromBool true  = I

  fun toBool O = false
    | toBool I = true

  exception Not_Binary
  fun fromNum 0 = O
    | fromNum 1 = I
    | fromNum _ = raise Not_Binary

  fun toNum O = 0
    | toNum I = 1

  (* Logical operators *)
  fun ! O = I
    | ! I = O

  infix 6 v
  fun O v O = O
    | _ v _ = I

  infix 7 &
  fun I & I = I
    | _ & _ = O

  (* XNOR is equality test *)
  infix 6 ==
  val op ==  = fromBool o op =

  infix 6 xor
  val op xor = op ! o fromBool o op =

  (* Signature related *)
  val make = map fromNum
  val dest = map toNum

  fun toString ys =
      let fun tS ([], str) = str
            | tS (O::xs, str) = tS (xs, str ^ "0")
            | tS (I::xs, str) = tS (xs, str ^ "1")
      in  tS (ys, "") end

  (* From and to integers *)
  fun fromPos 0 = [O]
    | fromPos 1 = [I,O]
    | fromPos n = fromNum (n mod 2)::fromPos (n div 2)

  fun fromNeg (0, _)     = [I]
    | fromNeg (1, false) = [I]
    | fromNeg (n, false) = let val bit = n mod 2 in
        fromNum bit::fromNeg (n div 2, bit = 1) end
    | fromNeg (n, true)  = ! (fromNum (n mod 2))::fromNeg (n div 2, true)

  fun fromInt n = if n < 0 then fromNeg (~n, false) else fromPos n

  fun toInt [I] = ~1
    | toInt []  = 0
    | toInt (b::bs) = (toNum b) + 2*toInt bs
 
  (* Wikipedia article: start with LSB, copy 0s til first 1, after which
   * flip rest (with appropriate sign and magnitude MSB.) *)
  fun neg []      = []
    | neg [I,O]   = [I]
    | neg (O::xs) = O::neg xs
    | neg (I::xs) =
      let fun flip []  = [O]
            | flip [x] = [I]
            | flip (x::xs) = !x::flip xs
      in  I::flip xs end

  (* Prep for arithmetic operations *)
  fun toLength (_, 0)     = []
    | toLength ([x], n)   = x::toLength ([x], n-1)
    | toLength (x::xs, n) = x::toLength (xs, n-1)
    | toLength ([], _)    = raise Empty

  fun equalise (a,b) =
    let val maxL = Int.max (length a, length b)
    in  (toLength (a, maxL), toLength (b, maxL)) end

  infix ##
  fun O ## [O] = [O]
    | I ## [I] = [I]
    | b ## bs  = b::bs

  (* For difference, changed and simplified add (a, !b, c).
   * Remember the initial carry for diff! *)
  fun add (a, b, c) = (c xor a xor b, a & b v c & (a xor b))
  fun sub (a, b, c) = (c == (a xor b), a & !b v c & (a == b))

  fun logic f ([], [], c)          = []
    | logic f ([], _, c)           = raise Empty
    | logic f (_, [], c)           = raise Empty
    | logic f (x::xs, y::ys, c_in) =
      let val (s, c_out) = f (x, y, c_in)
      in  s ## logic f (xs, ys, c_out) end

  fun sum (a,b)  = let val (x,y) = equalise (a,b) in logic add (x,y,O) end
  fun diff (a,b) = let val (x,y) = equalise (a,b) in logic sub (x,y,I) end

  (* Multiplication, tried Booth's but too much effort to keep list same size. *)
  fun times ([], b)    = []
    | times ([O], b)   = [O]
    | times ([I], b)   = neg b
    | times (O::xs, b) = O ## times (xs,b)
    | times (I::xs, b) = sum (b, O ## times (xs,b))

  fun prod (a,b) = if length a <= length b then times (a,b) else times (b,a)

  (* Slight cheating maybe, but considering term by term comparisons, signs,
   * unequal length numbers, this is definitely the most efficient. *)
  fun compare (a, b) = Int.compare (toInt a, toInt b)
  fun eq (a,b) = compare (a,b) = EQUAL
  fun abs a = (case List.last a of O => a | I => neg a)

  (* Simple repeated subtraction because it is 02:48. *)
  fun divide (dvnd, dvsr, q) =
    let val diffs = diff (dvnd, dvsr)
    in  if compare (diffs, zero) <> GREATER
          then (q, dvnd) else divide (diffs, dvsr, sum (q, one)) end

  fun quorem (a,b) = divide (a, b, zero)
  fun divd (a,b)   = #1(quorem(a,b))
  and rem (a,b)    = #2(quorem(a,b))

  fun gcd (m,n) = if m = zero then n else gcd (rem (m,n), n)
  end;
