(**** ML Programs from Chapter 7 of

  ML for the Working Programmer, 2nd edition
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1996)

Copyright (C) 1996 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)

(**** Functors ****)

(*** Matrix operations ***)

signature ZSP =
   sig
   type t
   val zero : t
   val sum  : t * t -> t
   val prod : t * t -> t
   end;


functor MatrixZSP (Z: ZSP) : ZSP =
  struct

  type t = Z.t list list;

  val zero = [];

  fun sum (rowsA,[]) = rowsA
    | sum ([],rowsB) = rowsB
    | sum (rowsA,rowsB) = ListPair.map (ListPair.map Z.sum) (rowsA,rowsB);

  fun dotprod pairs = foldl Z.sum Z.zero (ListPair.map Z.prod pairs);

  fun transp ([]::_) = []
    | transp rows = map hd rows :: transp (map tl rows);

  fun prod (rowsA,[]) = []
    | prod (rowsA,rowsB) = 
	let val colsB = transp rowsB 
	in  map (fn row => map (fn col => dotprod(row,col)) colsB) rowsA
	end;

  end;

structure IntZSP =
  struct
  type t = int;
  val zero = 0;
  fun sum   (x,y) = x+y: t;
  fun prod  (x,y) = x*y: t;
  end;

structure BoolZSP =
  struct
  type t = bool;
  val zero = false;
  fun sum   (x,y) = x orelse y;
  fun prod  (x,y) = x andalso y;
  end;

(** All-Pairs Shortest Paths -- Chapter 26 of Cormen et al. **)

(*Requires a maximum integer for INFINITY*)
structure PathZSP =
  struct
  type t = int;
  val zero = getOpt (Int.maxInt,1000000000)
  val sum  = Int.min
  fun prod(m,n) = if m=zero orelse n=zero then zero
                  else m+n;
  end;

structure PathMatrix = MatrixZSP (PathZSP);

fun fast_paths mat =
  let val n = length mat
      fun f (m,mat) = if n-1 <= m then mat
                       else f(2*m, PathMatrix.prod(mat,mat))
  in  f (1, mat)  end;

val zz = PathZSP.zero;

fast_paths[[0,  3,  8,  zz, ~4], 
	   [zz, 0,  zz, 1,   7], 
	   [zz, 4,  0,  zz,  zz], 
	   [2,  zz, ~5, 0,   zz], 
	   [zz, zz, zz, 6,   0]];
