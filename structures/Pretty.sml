(**** ML Programs from Chapter 8 of

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

structure Pretty : PRETTY =
  struct
  (*Printing items: compound phrases, strings, and breaks*)
  datatype t = 
      Block of t list * int * int 	(*indentation, length*)
    | String of string
    | Break of int;			(*length*)

  (*Add the lengths of the expressions until the next Break; if no Break then
    include "after", to account for text following this block. *)
  fun breakdist (Block(_,_,len)::es, after) = len + breakdist(es, after)
    | breakdist (String s :: es, after) = size s + breakdist (es, after)
    | breakdist (Break _ :: es, after) = 0
    | breakdist ([], after) = after;

  (* Functional *) 
  fun toString (e, margin) =
    let fun blanks 0 = "" | blanks n = " " ^ blanks (n-1)

        fun printing ([], _, _, sp, r) = (r, sp)
          | printing (e::es, blocksp, after, sp, r) =
          let val (res, sp') =

            case e of 
              Block (bes, ind, len) =>
                printing (bes, sp-ind, breakdist (es, after), sp, "")
            | String s   => (s, sp - size s)
            | Break  len =>
                if len + breakdist (es, after) <= sp then
                  (blanks len, sp-len)
                else
                  ("\n" ^ blanks (margin-blocksp), blocksp)

          in printing (es, blocksp, after, sp', r^res) end

    in  #1(printing ([e], margin, 0, margin, "")) end

  (* Imperative *)
  fun pr (os, e, margin) =
   let
     val space = ref margin

     fun blanks n = (TextIO.output(os, StringCvt.padLeft #" " n "");  
             space := !space - n)

     fun newline () = (TextIO.output(os,"\n");  space := margin)

     fun printing ([], _, _) = ()
       | printing (e::es, blockspace, after) =
        (case e of
             Block(bes,indent,len) =>
               printing (bes, !space-indent, breakdist (es,after))
           | String s  => (TextIO.output(os,s);   space := !space - size s)
           | Break len => 
               if len + breakdist(es,after) <= !space then
                 blanks len
               else
                 (newline();  blanks(margin-blockspace));
          printing (es, blockspace, after))
   in
     printing([e], margin, 0);  newline()
   end;

  fun length (Block(_,_,len)) = len
    | length (String s) = size s
    | length (Break len) = len;

  val str = String  and  brk = Break;

  fun blo (indent,es) =
    let fun sum ([], k) = k
	  | sum (e::es, k) = sum (es, length e + k)
    in  Block(es, indent, sum (es,0))  end;
  end;
