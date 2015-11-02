signature TREE = 
  sig
  datatype traverse = PRE | IN | POST
  datatype style = HORZ | VERT 

  val ordIn:   traverse option
  val ordPre:  traverse option
  val ordPost: traverse option

  val horz:    style option
  val vert:    style option

  val size:     'a tree -> int
  val depth:    'a tree -> int
  val reflect:  'a tree -> 'a tree
  val map:      ('a -> 'b) -> 'a tree -> 'b tree
  val toList:   traverse option -> 'a tree -> 'a list
  val fromList: traverse option -> 'a list -> 'a tree
  val toString: style option -> ('a -> string) -> 'a tree -> string
  end;
