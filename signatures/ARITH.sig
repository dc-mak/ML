signature ARITH =
  sig
  type t'
  type t
  val zero : t

  val make : t' -> t
  val dest : t  -> t'
  val toString : t -> string

  val neg  : t -> t
  val sum  : t * t -> t
  val diff : t * t -> t
  val prod : t * t -> t
  val divd : t * t -> t
  val eq   : t * t -> bool
  end;

signature NUMBER =
  sig
  include ARITH
  val one : t
  val abs : t -> t
  val compare : t * t -> order
  end;
