(* Dhruv Makwana, Trinity College, dcm41 *)

(* Q1 *)
fun last [] = raise Empty
  | last [x] = x
  | last (x::xs) = last xs;

(* Q2 *)
fun butLast [] = []
  | butLast [x] = []
  | butLast (x::xs) = x::butLast xs;

(* Q3 *)
fun nth ([], n)     = raise Subscript
  | nth (x::xs, 0)  = x
  | nth (x::xs, n)  = nth (xs, n-1);

last [1,2,3,4];
last ["poop", "shit", "crap", "excrement", "scat"];
butLast [1,2,3,4];
nth ([2,3,5,7], 2);

(*
 * val last = fn: 'a list -> 'a
 * val butLast = fn: 'a list -> 'a list
 * val nth = fn: 'a list * int -> 'a
 * val it = 4: int
 * val it = "scat": string
 * val it = [1, 2, 3]: int list
 * val it = 5: int
 *)
