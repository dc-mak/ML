(* TODO: Draw out hierarchy of Tree, Order and Dictionary before proceeding. *)
signature ORD_DICTIONARY =
  sig
  structure Order : ORDER
  structure Dict  : DICTIONARY
  type key
  type 'a t
  exception E of key
  val empty: 'a t
  val lookup: 'a t * key -> 'a
  val insert: 'a t * key * 'a -> 'a t
  val update: 'a t * key * 'a -> 'a t
  val union : 'a t * 'a t -> 'a t
  val delete: 'a t * 'a -> 'a t
  end;

