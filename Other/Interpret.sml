(* 1998, Paper 13, Q8 *)
(* Dhruv Makwana *)

datatype expression = Expr of (string -> int) -> int;
(* If you give it a function, it will give you an integer *)

datatype command = Assign of (string * expression)
                 | Sequence of (command * command)
                 | While_do of (expression * command);

val fp = Sequence(Assign("x", Expr(fn s => 1)),
            While_do(Expr(fn s => s"n"),
                Sequence(Assign("x", Expr(fn s => s"x" * s"n")),
                    Assign("n", Expr(fn s => s"n" - 1)))));

(* So this is the way we store variable values throughout the execution of the
* program: we're extending the function with pattern-matching (and overriding).
* So if the input string matches x, then return i (the pre-computed and "stored"
* assigned value).  Otherwise, check all the other functions/patterns. *)
fun update (s,x,i) = fn a => if a = x then i else s a;

(* run update for assign, conceptually simple *)
fun interpret (Assign (s, Expr e)) f = update(f, s, e f)
  | interpret (w as While_do (Expr e, cmd)) f =
  (* trippy shit *)
    if (e f) = 0 then f else interpret (Sequence (cmd, w)) f
  (* Simple recursive chaining based on the types they're meant to be *)
  | interpret (Sequence(cmd1, cmd2)) f = interpret cmd2 (interpret cmd1 f)
