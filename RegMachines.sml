(* Arbitrary register machines *)
type reg = int
type loc = int

datatype instr =
  (* reg++; goto loc *)
  Inc  of reg * loc         
  (* if reg > 0 then reg--; goto loc1 else goto loc2 *)
| Cond of reg * loc * loc   
  (* stop execution *)
| HALT

exception ERR of int

fun eval prog regs =
    let val nth = List.nth
        val its = Int.toString

        fun inc (r::rs, 0) = (r+1)::rs
          | inc (r::rs, n) = r::inc (rs, n-1)

        fun dec (r::rs, 0) = if r > 0 then ((r-1)::rs, true) else (0::rs, false)
          | dec (r::rs, n) = let val (rs, c) = dec (rs, n-1) in (r::rs, c) end

        fun run (rs, HALT) = rs 
          | run (rs, Inc (r,l)) = run (inc (rs, r), nth(prog, l))

          | run (rs, Cond (r, l1, l2)) =
            let val (rs, pos) = dec (rs, r)
            in  run (rs, nth (prog, if pos then l1 else l2)) end

        fun split xs = (hd xs,xs)

    in  (split o run) (regs, hd prog) end

local val (r,x,y) = (0,1,2) in
(* (0, x, y) => (x+y, 0, y) *)
val add  = [ Cond (x,1,2), (* 0 *)
               Inc  (r,0), (* 1 *)
             Cond (y,3,4), (* 2 *)
               Inc  (r,2), (* 3 *)
             HALT          (* 4 *)]
end


local val (r,x,y,y1) = (0,1,2,3) in
(* (0, x, y, 0) => (xy, 0, y, 0) *)
val mult = [ Cond (x,1,6),    (* 0 *)

               Cond (y,2,4),  (* 1 *)
                 Inc  (r,3),  (* 2 *)
                 Inc  (y1,1), (* 3 *)

               Cond (y1,5,0), (* 4 *)
                 Inc  (y,4),  (* 5 *)

             HALT             (* 6 *)]
end

local val (r,x) = (0,1) in
(* (0, x, ...) => (x, 0, ...) *)
val proj = [ Cond (x,1,2), (* 0 *)
               Inc  (r,0), (* 1 *)
             HALT          (* 2 *)]
end

val const = [HALT]

local val (r,x,y) = (0,1,2) in
(* (0, x, y) => (x-y, n, _) where n = x < y *)
val sub  = [ Cond (x,1,2),   (* 0 *)
               Inc  (r,0),   (* 1 *)
             Cond (y,3,5),   (* 2 *)
               Cond (r,2,4), (* 3 *)
                 Inc  (x,5), (* 4 *)
             HALT            (* 5 *)]
 end

local val (r,x,y1,y2) = (0,1,2,3) in
(* (0, x, y, 0) => (x div y, 0, y, 0) *)

(*       if y = 0 then 0
 *       else if x = 0 then 0
 * loop: else x - y, r++
 *       if x < 0 then r--, HALT
 *       else loop                *)
val divd = [ Cond (y1,1,10),  (* 0 *)
             Inc  (y1,2),     (* 1 *)

             Cond (x,3,10),   (* 2 *)
               Inc  (x,4),    (* 3 *)
               Cond (y1,5,7), (* 4 *)
                 Inc  (y2,6), (* 5 *)

               Cond (x,4,10), (* 6 *)

             Cond (y2,8,9),   (* 7 *)
               Inc  (y1,7),   (* 8 *)

             Inc  (r,2),      (* 9 *)
             HALT             (* 10 *)]
end

local val (r0,x0,y0,x1,y1) = (0,1,2,3,4) in
(* (0,x,y,0,0,0) => (x mod y,0,_,0,_,0) *)

(*        if y = 0 then 0
*         else if x = 0 then 0
*   loop: else x1=r=x0, x0=y0-x0, 
*         if x0 < 0 then r
*         else r=0, loop          *)
val modo = [ Cond (y0,1,15),   (* 0 *)
             Inc  (y0,2),      (* 1 *)
             Cond (x0,3,15),   (* 2 *)
             Inc  (x0,4),      (* 3 *)

             Cond (x0,5,7),    (* 4 *)
               Inc  (x1,6),    (* 5 *)
               Inc  (r0,4),    (* 6 *)

             Cond (x1,8,9),    (* 7 *)
               Inc  (x0,7),    (* 8 *)

             Cond (y0,10,12),  (* 9 *)
               Inc  (y1,11),   (* 10 *)
               Cond (x0,9,15), (* 11 *)

            Cond (y1,13,14),   (* 12 *)
              Inc  (y0,12),    (* 13 *)

            Cond (r0,14,4),    (* 14 *)

            HALT               (* 15 *)]
end

local val (r,x,r1,r2) = (0,1,2,3) in
(* (0, x, 0, 0) => (2^x, 0, 0, 0) *)

(* loop: if x=0 then 1
 * else r0=2*r0, x--, loop *)
val exp2 = [ Inc  (r,1),      (* 0 *)

             Cond (x,2,9),    (* 1 *)
               Cond (r,3,5),  (* 2 *)
                 Inc  (r1,4), (* 3 *)
                 Inc  (r2,2), (* 4 *)

               Cond (r1,6,7), (* 5 *)
                 Inc  (r,5),  (* 6 *)

               Cond (r2,8,1), (* 7 *)
                 Inc  (r,7),  (* 8 *)

             HALT             (* 9 *)]
end

local val (r,x,a) = (0,1,2) in
(* (0, x, 0, 0) => (lg x, 0, 0) *)

(* if x = 1 then 0 else
 * a=x/2, r++, b=0, x=a and recurse *)
val log2 = [ Cond (x,1,10),  (* 0 *) 
             Cond (x,2,10),  (* 1 *)
             Inc  (x,3),     (* 2 *)
             Inc  (x,4),     (* 3 *)

             Cond (x,5,7),   (* 4 *)
               Cond (x,6,7), (* 5 *)
               Inc  (a,4),   (* 6 *)
             
             Inc  (r,8),     (* 7 *)
             Cond (a,9,0),   (* 8 *)
               Inc  (x,8),   (* 9 *)
             
             HALT            (* 10 *)]
end

(* Not necessary for Poly/ML *)

local open IntInf (* arbitrary precision integers *)

  val inf   = fromInt

  val zero  = inf 0
  and one   = inf 1
  fun two x = inf 2 * x

  fun half j = j div (inf 2) 
  and odd  j = j mod (inf 2)=one
  and even j = j mod (inf 2)=zero

  fun pw2 x =
    let fun p2 (x,n) = if x=zero then n else p2 (x-one, two n) in p2 (x, one) end

  fun split x = 
    let fun cut (i,j) =
          if odd j then (i, half j) else cut (i+one, half j)
    in cut (zero, x) end

  fun inc (i,j) = Inc (toInt i, toInt j)
  fun cond (i, j, k) = Cond (toInt i, toInt j, toInt k)

in 

  val inf = inf

  fun <<>> (x,y) = pw2 x * (two y + one)
  fun <##> xy    = <<>> xy - one

  fun >>  HALT            = zero
    | >> (Inc (i,j))      = <<>> (two (inf i), inf j)
    | >> (Cond (i,j,k))   = <<>> (two (inf i) + one, <##> (inf j, inf k))

  fun $$ []      = zero
    | $$ (x::xs) = <<>> (x, $$ xs)

  fun godel []      = zero
    | godel (x::xs) = <<>> (>> x, godel xs)
    
  fun toList x = if x=zero then [] else case split x of (i,j) => i::toList j

  fun toInstr x =
          if x=zero then HALT else
          let val (y,z) = split x
              val i     = half y
          in  if even y then inc (i,z) else
                 case split (z+one) of (j,k) => cond (i, j, k) end

  fun toProg x =
    if x=zero then [] else case split x of (i,j) => toInstr i::toProg j

end
