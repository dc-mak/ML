(* A simple one would be abs (x-y) < macheps *)
signature FLOAT_COMPARE =
  sig
  val compare : real * real -> order
  end;

functor FloatFunctor (C : FLOAT_COMPARE) :> NUMBER where type t' = real =
  struct
  type t  = real
  type t' = real

  val zero = 0.0
  val one  = 1.0

  val make = fn x => x
  val dest = fn x => x
  val toString = Real.toString

  val neg  = Real.~
  val abs  = Real.abs
  val sum  = Real.+
  val diff = Real.-
  val prod = Real.*
  val divd = Real./

  val compare  = C.compare
  fun eq (x,y) = C.compare (x,y) = EQUAL
  end;
