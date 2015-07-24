signature PRIORITY_QUEUE =
  sig
  structure Item : ORDER
  type t
  val empty    : t
  val null     : t -> bool
  val insert   : Item.t * t -> t
  val min      : t -> Item.t
  val delmin   : t -> t
  val fromList : Item.t list -> t
  val toList   : t -> Item.t list
  val sort     : Item.t list -> Item.t list
  end;
