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

(*** Propositional logic -- tautology checker ***)

(*REQUIRES: inter from chapter 3*)



datatype prop = 
    Atom of string
  | Neg  of prop
  | Conj of prop * prop
  | Disj of prop * prop;

fun show (Atom a) = a
  | show (Neg p) = "(~ " ^ show p ^ ")"
  | show (Conj(p,q)) = "(" ^ show p ^ " & " ^ show q ^ ")"
  | show (Disj(p,q)) = "(" ^ show p ^ " | " ^ show q ^ ")";

(*naive version*)
fun nnf (Atom a) = Atom a
  | nnf (Neg (Atom a)) = Neg (Atom a)
  | nnf (Neg (Neg p)) = nnf p
  | nnf (Neg (Conj(p,q))) = nnf (Disj(Neg p, Neg q))
  | nnf (Neg (Disj(p,q))) = nnf (Conj(Neg p, Neg q))
  | nnf (Conj(p,q)) = Conj(nnf p, nnf q)
  | nnf (Disj(p,q)) = Disj(nnf p, nnf q);

fun nnfpos (Atom a) = Atom a
  | nnfpos (Neg p) = nnfneg p
  | nnfpos (Conj(p,q)) = Conj(nnfpos p, nnfpos q)
  | nnfpos (Disj(p,q)) = Disj(nnfpos p, nnfpos q)
and nnfneg (Atom a) = Neg (Atom a)
  | nnfneg (Neg p) = nnfpos p
  | nnfneg (Conj(p,q)) = Disj(nnfneg p, nnfneg q)
  | nnfneg (Disj(p,q)) = Conj(nnfneg p, nnfneg q);

fun distrib (p, Conj(q,r)) = Conj(distrib(p,q), distrib(p,r))
  | distrib (Conj(q,r), p) = Conj(distrib(q,p), distrib(r,p))
  | distrib (p, q) = Disj(p,q)   (*no conjunctions*) ;

fun cnf (Conj(p,q)) = Conj (cnf p, cnf q)
  | cnf (Disj(p,q)) = distrib (cnf p, cnf q)
  | cnf p = p    (*a literal*) ;

exception NonCNF;

fun positives (Atom a)      = [a]
  | positives (Neg(Atom _)) = []
  | positives (Disj(p,q))   = positives p @ positives q
  | positives  _            = raise NonCNF;

fun negatives (Atom _)      = []
  | negatives (Neg(Atom a)) = [a]
  | negatives (Disj(p,q))   = negatives p @ negatives q
  | negatives  _            = raise NonCNF;

fun taut (Conj(p,q)) = taut p andalso taut q
  | taut p = not (null (inter (positives p, negatives p)));
