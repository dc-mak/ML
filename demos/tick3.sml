(* IA ML/Java demonstrator, Dhruv Makwana *)

datatype 'a tree = Lf | Br of 'a * 'a tree * 'a tree;

(* Q1 *)
(* Chapter 4: Trees and Concrete Data, p156. tcons = tree_cons = insert at front *)
fun tcons (v, Lf)            = Br (v, Lf, Lf)
  | tcons (v, Br(w, left, right)) = Br(v, tcons (w, right), left)

(* Expected *)
fun arrayOfList []           = Lf
  | arrayOfList (head::tail) = tcons(head, arrayOfList tail)

(* Uber-short demonstrator show-off version *)
fun arrayOfList xs = foldr tcons Lf xs

(* Alternative way, without tcons *)
fun split []               = ([], [])
  | split [x]              = ([x], [])
  | split (fst::snd::rest) =
    let val (left, right)  = split rest
    in  (fst::left, snd::right) end

fun fromList []           = Lf
  | fromList [x]          = Br(x, Lf, Lf)
  | fromList (head::tail) =
    let val (l, r)        = split tail
        val (left, right) = (fromList l, fromList r)
    in  Br(head, left, right) end

(* Q2 *)
fun inter (left, [])          = left
  | inter ([], right)         = right
  | inter (l::left, r::right) = l::r::inter(left, right)

fun listOfArray Lf                   = []
  | listOfArray (Br(x, left, right)) =
      x::inter(listOfArray left, listOfArray right)

(* Q3 *)
fun getSubsOfEvens Lf            = []
  | getSubsOfEvens (Br(x, l, r)) =
  let val left  = map (fn x => 2*x) (getSubsOfEvens l)
      val right = map (fn x => 2*x+1) (getSubsOfEvens r)
      val soFar = inter(left, right)
  in if x mod 2 = 0 then  x::soFar else soFar end

(* Alternative *)
fun getS2 tree =
  let fun buildEven (n, []) = []
        | buildEven (n, [x]) = if x mod 2 = 0 then [x] else []
        | buildEven (n, h::tail) =
          let val soFar = buildEven(n+1, tail)
          in  if h mod 2 = 0 then n::soFar else soFar end
  in  buildEven (1, listOfArray tree) end
