(**** ML Programs from Chapter 4 of

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

structure Tree :> TREE =
  struct
  datatype traverse = PRE | IN | POST
  datatype style = HORZ | VERT

  val ordIn = SOME IN
  val ordPre = SOME PRE
  val ordPost = SOME POST
  val horz = SOME HORZ
  val vert = SOME VERT

  fun size Lf = 0
    | size (Br(v,l,r)) = 1 + size l + size r;

  fun depth Lf = 0
    | depth (Br(v,l,r)) = 1 + Int.max (depth l, depth r);

  fun reflect Lf = Lf
    | reflect (Br(v,l,r)) = 
      Br(v, reflect r, reflect l);

  fun map f Lf    = Lf
    | map f (Br(x, l, r)) = Br(f x, map f l, map f r);

  fun preord (Lf, vs) = vs
    | preord (Br(v,l,r), vs) =
      v :: preord (l, preord (r, vs));

  fun inord (Lf, vs) = vs
    | inord (Br(v,l,r), vs) =
      inord (l, v::inord (r, vs));

  fun postord (Lf, vs) = vs
    | postord (Br(v,l,r), vs) =
      postord (l, postord (r, v::vs));

  fun toList (SOME PRE)  tree = preord  (tree, [])
    | toList  NONE       tree = inord   (tree, [])
    | toList (SOME IN)   tree = inord   (tree, [])
    | toList (SOME POST) tree = postord (tree, [])

  fun split ([], len)    = ([], [], len div 2)
    | split (x::xs, pos) = case split (xs, pos+1) of (fs, bs, half) =>
        if pos < half then (x::fs, bs, half) else (fs, x::bs, half)

  fun fromPre []      = Lf
    | fromPre (x::xs) = case split (xs, 0) of
         (left, right, _) => Br(x, fromPre left,  fromPre right)

  fun fromIn [] = Lf
    | fromIn xs = case split (xs, 0) of
         (left, r::right, _) => Br(r, fromIn left, fromIn right)

  fun fromPost xs = reflect (fromPre (rev xs))

  fun fromList (SOME PRE)  xs = fromPre  xs
    | fromList  NONE       xs = fromIn   xs
    | fromList (SOME IN)   xs = fromIn   xs
    | fromList (SOME POST) xs = fromPost xs

  fun secr f y x   = f (x, y)

  fun toHorz toStr tree = let structure S = Substring

      val sub = S.full o toStr
      val sp  = S.full o implode o secr List.tabulate (fn i => #" ")
      fun toSub x = case sub x of s => (s, sp (S.size s))
      val nl  = S.full "\n"
      
      fun tag (Lf, _)              = Lf
        | tag (Br(x, l, r), depth) = case toSub x of (sub, blanks) =>
            Br ((sub, blanks, depth), tag (l, depth+1), tag(r, depth+1))

      val tlist = ((secr inord []) o (secr tag 0)) tree
      val depth = foldl (fn (v,e) => Int.max (#3(v), e)) 0 tlist
      fun cvt ((sub, blanks, level), d) = if d = level then sub else blanks

      fun write d = if d > depth then [] else
            foldr (fn (x,e) => cvt (x,d)::e) [nl, nl] tlist::write (d+1)

    in S.concatWith " " (List.concat (write 0)) end

  fun toVert toStr tree = let

        val blanks = implode o secr List.tabulate (fn i => #" ")

        fun toV (Lf, ind)          = ""
          | toV (Br(x, l, r), ind) =
            let val s = blanks ind ^ toStr x ^ "\n"
                val n = 1 + String.size s
            in  toV (l, n) ^ s ^ toV (r, n) end

    in  toV (tree, 0) end

  fun toString  NONE       toStr tree = toVert toStr tree
    | toString (SOME VERT) toStr tree = toVert toStr tree
    | toString (SOME HORZ) toStr tree = toHorz toStr tree

  end;
