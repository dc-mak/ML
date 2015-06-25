(*#!/usr/bin/env poly --script
PolyML.print_depth 100;*)

datatype 'a tree = Twig of 'a
                 | Br of 'a * 'a tree * 'a tree;

val given = Br(1, Br(4, Twig 7, Twig 9), Br(2, Twig 4, Twig 3));

fun prime n =
let
  fun from_to a b acc = if a > b then acc else from_to a (b-1) (b::acc);
  val test = from_to 2 (Real.floor (Math.sqrt (Real.fromInt n))) [];
in
  n > 1 andalso not (List.exists (fn x => n mod x = 0) test)
end;

exception NoPath;

(*fun find_path p (Twig(v)) = if p v then [v] else raise NoPath
  | find_path p (Br(v, l, r)) =
  let
    val p' = (fn x => p (x+v))
  in
    v::(find_path p' l handle NoPath => find_path p' r)
  end;*)

fun find_path p t =
let
  fun find_path_opt p (Twig(v)) = if p v then SOME([v]) else NONE
    | find_path_opt p (Br(v, l, r)) =
    let
      val p' = (fn x => p (x+v))
      val left = find_path_opt p' l
      val opt = if isSome left then left else find_path_opt p' r
    in
      Option.map (fn path => v::path) opt
    end;
in
  case find_path_opt p t of
       NONE => raise NoPath
     | SOME(path) => path
end;

find_path prime given;

fun all_paths p (Twig(v)) = if p v then [[v]] else []
  | all_paths p (Br(v, l, r)) =
  let
    val p' = (fn x => p (x+v))
    val lpaths = all_paths p' l
    val rpaths = all_paths p' r
  in
    map (fn path => v::path) (lpaths @ rpaths)
  end;

all_paths (fn x => x mod 2 = 0) given;

datatype 'a seq = Void | Cons of 'a * (unit -> 'a seq);

(* Second argument is a lazy lazy list *)
(* 'a seq -> (unit -> 'a seq) -> 'a seq *)
fun vlconcat Void bs = bs ()
  | vlconcat (Cons(a, as_)) bs = Cons(a, fn () => vlconcat (as_()) bs);

fun lmap f Void = Void
  | lmap f (Cons(a, as_)) = Cons(f a, fn () => lmap f (as_()));

fun all_pathq p (Twig(v)) = if p v then Cons([v], fn () => Void) else Void
  | all_pathq p (Br(v, l, r)) =
  let
    val p' = (fn x => p (x+v))
    val lpaths = all_pathq p' l
    val rpaths = fn () => all_pathq p' r
  in
    (print "allpathq\n"; lmap (fn path => v::path) (vlconcat lpaths rpaths))
  end;

fun listify Void = []
  | listify (Cons(a, as_)) = a :: (listify (as_()));

fun pow2 0 = 1
  | pow2 n = 2 * (pow2 (n-1));

fun construct height =
let
  fun inner_construct 0 v = Twig v
    | inner_construct height v = Br(v, inner_construct (height-1) (v+1), inner_construct (height-1) (v + (pow2 height)));
in
  inner_construct height 0
end;

val bigtree = construct 10;

(*val paths = all_pathq (fn x => x mod 2 = 0) bigtree;
listify paths;*)
listify (all_pathq prime bigtree);

