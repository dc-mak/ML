signature TREE = 
  sig
  datatype 'a tree = Lf  |  Br of 'a * 'a tree * 'a tree
  val size: 'a tree -> int
  val depth: 'a tree -> int
  val reflect: 'a tree -> 'a tree
  end;
