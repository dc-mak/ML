(* TODO: Corresponding structure and propogate extra functions available 
 *       in tree to Braun and FlexArray. Remember, not necessarily key-value
 *       pair, so keep it general. *)
signature ORD_TREE =
  sig
  structure Order : ORDER
  structure Tree  : TREE
  val update : Order.t tree * Order.t -> Order.t tree
  val insert : Order.t tree * 
  val lookup : Order.t tree * Order.t -> t
  val union  : Order.t tree * Order.t tree -> Order.t tree
  val delete : Order.t tree * Order.t -> Order.t tree
  end;
