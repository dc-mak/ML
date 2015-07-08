(**** ML Programs from Chapter 3 of

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

(*** Graph algorithms ***)

fun nexts (a, []) = []
  | nexts (a, (x,y)::pairs) =
      if a=x then  y :: nexts(a,pairs)
             else       nexts(a,pairs);

fun depthf ([], graph, visited) = rev visited
  | depthf (x::xs, graph, visited) =
      if x mem visited then depthf (xs, graph, visited)
      else depthf (nexts(x,graph) @ xs, graph, x::visited);

(*Alternative, faster function for depth-first search*)
fun depth ([], graph, visited) = rev visited
  | depth (x::xs, graph, visited) =
      depth (xs, graph, 
             if x mem visited  then  visited
             else depth (nexts(x,graph), graph, x::visited));

fun topsort graph =
  let fun sort ([], visited) = visited
        | sort (x::xs, visited) =
            sort(xs, if x mem visited  then  visited
                     else x :: sort(nexts(x,graph), visited));
      val (xs,_) = ListPair.unzip graph
  in sort(xs, []) end;

fun pathsort graph =
  let fun sort ([], path, visited) = visited
        | sort (x::xs, path, visited) =
            if x mem path then hd[] (*abort!!*)
            else sort(xs, path,
                      if x mem visited  then  visited
                      else x :: sort(nexts(x,graph), x::path, visited))
      val (xs,_) = ListPair.unzip graph
  in sort(xs, [], []) end;


fun newvisit (x, (visited,cys)) = (x::visited, cys);

fun cyclesort graph =
  let fun sort ([], path, (visited,cys)) = (visited, cys)
        | sort (x::xs, path, (visited,cys)) =
            sort(xs, path, 
               if x mem path   then  (visited, x::cys)
               else if x mem visited then (visited, cys)
               else newvisit(x, sort(nexts(x,graph),
                                     x::path, (visited,cys))))
      val (xs,_) = ListPair.unzip graph
  in sort(xs, [], ([],[])) end;
