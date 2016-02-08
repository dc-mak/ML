(* Dhruv Makwana: 1997.1.2: Foundations of CS *)

(* Verbose but readable datatypes *)
datatype player = O | X
datatype cell = Empty | Played of player
datatype row = Row of cell * cell * cell
datatype board = Board of row * row * row
datatype 'a tree = Lf of 'a | Tr of 'a * tree list

(* Generate all possible moves in a row *)
fun moveRow m (Row (a,b,c)) = let

    val r1 = if a = Empty then [Row(Played m, b,c)] else []
    and r2 = if b = Empty then [Row(a, Played m,c)] else []
    and r3 = if c = Empty then [Row(a,b, Played m)] else []

  in r1 @ r2 @ r3 end

(* Add moves to board *)
fun addMove (p, b as Board(r1, r2, r3)) = let
                                  
    val newRow = moveRow p
    val b1 = map (fn new => Board(new, r2, r3)) (newRow r1)
    and b2 = map (fn new => Board(r1, new, r3)) (newRow r2)
    and b3 = map (fn new => Board(r1, r2, new)) (newRow r3)

  in b1 @ b2 @ b3 end

fun allPlayed (Board(Row(Played _, Played _, Played _), 
                     Row(Played _, Played _, Played _),
                     Row(Played _, Played _, Played _))) = true
  | allPlayed _                                          = false

(* Check rows, columns and diagonals *)
fun someWin (Board (Row(r1 as (a,d,g)),
                    Row(r2 as (b,e,h)),
                    Row(r3 as (c,f,i)))) =
  let 

    fun allSame (Played a, Played b, Played c) =
          if a=b andalso a=c then SOME a else NONE
      | allSame _                           = NONE

  in
    (* rows *)
    case allSame r1 of (p as SOME _) => p | _ =>
    case allSame r2 of (p as SOME _) => p | _ =>
    case allSame r3 of (p as SOME _) => p | _ =>
    (* columns *)
    case allSame (a,b,c) of (p as SOME _) => p | _ =>
    case allSame (d,e,f) of (p as SOME _) => p | _ =>
    case allSame (g,h,i) of (p as SOME _) => p | _ =>
    (* diagonals *)
    case allSame (a,e,i) of (p as SOME _) => p | _ =>
         allSame (g,e,c)

  end

(* Win or draw *)
fun gameOver b = someWin b <> NONE orelse allPlayed b

(* Generate tree *)
fun mktree () = let

    fun make p b = if gameOver b then Lf b else
      let val p' = if p = O then X else O
      in  Tr(b, map (make p') (addMove (p, b))) end

    val e = Empty
    val r = Row (e, e, e)
    val b = Board (r, r, r)

  in make O b end

fun oWins (Lf b) =  (case someWin b of SOME O => 1 | _ => 0)
  | oWins (Tr (_, boardList)) = foldl op+ 0 (map oWins boardList)

val test = oWins o mktree
