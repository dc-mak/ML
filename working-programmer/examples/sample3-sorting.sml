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

(*** Sorting ***)

(** Random numbers, courtesy Stephen K. Park and Keith W. Miller, 
	CACM 31 (1988), 1192-1201.  **)
local val a = 16807.0  and  m = 2147483647.0
in  fun nextrandom seed =
          let val t = a*seed
          in  t - m * real(floor(t/m))  end

    (*truncate to integer from 1 to k*)
    and truncto k r = 1 + floor((r / m) * (real k))
end;

fun randlist (n,seed,seeds) =
    if n=0  then  (seed,seeds)  
    else  randlist(n-1, nextrandom seed, seed::seeds);


(** insertion sort: non-iterative is faster **)
fun ins (x, []): real list = [x]
  | ins (x, y::ys) = 
      if x<=y then x::y::ys    (*it belongs here*)
              else y::ins(x,ys);

fun insort [] = []
  | insort (x::xs) = ins(x, insort xs);


(*quicksort*)
fun quick [] = []
  | quick [x] = [x]
  | quick (a::bs) =  (*the head "a" is the pivot*)
      let fun partition (left,right,[]) : real list = 
                (quick left) @ (a :: quick right)
            | partition (left,right, x::xs) =
                if x<=a then partition (x::left, right, xs)
                        else partition (left, x::right, xs)
      in  partition([],[],bs)  end;


(** Top-down merge sort **)

fun merge([],ys) = ys : real list
  | merge(xs,[]) = xs
  | merge(x::xs, y::ys) =
      if x<=y then x::merge(xs,  y::ys)
              else y::merge(x::xs,  ys);

(*naive version -- like Bird and Wadler, following Sedgewick*)
fun tmergesort [] = []
  | tmergesort [x] = [x]
  | tmergesort xs =
      let val k = length xs div 2
      in  merge (tmergesort (List.take(xs,k)),
                 tmergesort (List.drop(xs,k)))
      end;

(*faster version*)
fun tmergesort' xs =
      let fun sort (0, xs) = ([], xs)
	    | sort (1, x::xs) = ([x], xs)
	    | sort (n, xs) =
		let val (l1, xs1) = sort ((n+1) div 2, xs)
		    val (l2, xs2) = sort (n div 2, xs1)
		in (merge (l1,l2), xs2)
		end
          val (l, _) = sort (length xs, xs)
      in l end;


(** Bottom-up merge sort **)

fun mergepairs([l], k) = [l]
  | mergepairs(l1::l2::ls, k) =
      if k mod 2 = 1 then l1::l2::ls
      else mergepairs(merge(l1,l2)::ls, k div 2);

fun sorting([], ls, r) = hd(mergepairs(ls,0))
  | sorting(x::xs, ls, r) = sorting(xs, mergepairs([x]::ls, r+1), r+1);

fun sort xs = sorting(xs, [[]], 0);

(*O'Keefe's samsort*)
fun nextrun(run, []) =       (rev run, []: real list)
  | nextrun(run, x::xs) =
        if  x < hd run then  (rev run, x::xs)
                       else  nextrun(x::run, xs);

fun samsorting([], ls, k) = hd(mergepairs(ls,0))
  | samsorting(x::xs, ls, k) = 
      let val (run, tail) = nextrun([x], xs)
      in  samsorting(tail, mergepairs(run::ls, k+1), k+1)
      end;

fun samsort xs = samsorting(xs, [[]], 0);
