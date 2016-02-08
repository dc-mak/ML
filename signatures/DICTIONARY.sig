(* TODO: Corresponding structure and appropriate application of Tree
 *       operations such as to/from List and or string.
 *       REMEMBER: Dictionary isn't necessarily a tree, so keep it general. *)
signature DICTIONARY = 
  sig
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
