(* 2010, Selection Sort and Tables *)
fun least (x,e) = if e < x then e else x;

(* Hack fix *)
infix --
fun [] -- y      = []
  | (x::xs) -- y = if x = y then xs else x::(xs -- y);

(* Hack fix does not work *)
fun selsort xs = let

  fun sel [] r      = r
    | sel (y::ys) r =
      let val min = foldl least y ys
      in  sel (ys--min) (min::r) end

in rev (sel xs []) end;

(* My new idea *)
fun min ys m [] = (m,ys)
  | min ys m (x::xs) =
    if m < x then min (x::ys) m xs else min (m::ys) x xs;
  
(* See, I *can* do this stuff given
 * a little bit of time... :(      *)
fun sel2 xs = let

  fun sel [] r      = rev r
    | sel (x::xs) r = case (min [] x xs) of (m,ys) => sel ys (m::r)

in sel xs [] end;

(* What I did *)
fun mTable n = let

  fun gen xs 0 r = r
    | gen xs m r = gen xs (m-1) ((map (fn x => x*m) xs)::r)

in gen (List.tabulate(n, fn i => i+1)) n [] end;

(* What I should have done *)
fun listTable n =
  List.tabulate(n, fn i =>
    List.tabulate(n, fn j => (i+1)*(j+1)));

(* What I did, after a while, also spectacularly wrong*)
fun mTable3D n = let

  fun gen2 0 r = r
    | gen2 m r = gen2 (m-1) ((mTable (m*n))::r)

in gen2 n [] end;

(* What I should have done *)
fun listTable3D f n =
  List.tabulate(n, fn i =>
    List.tabulate(n, fn j =>
      List.tabulate(n, fn k => f i j k)))

(* Larry Paulson mapping *)
fun table n = 
  let val seg = List.tabulate(n, fn i => i+1)
  in  map (fn i => map (fn j => i*j) seg) seg end;

fun table3D f n =
  let val seg = List.tabulate(n, fn i => i+1)
  in  map (fn i => map (fn j => map (fn k => f i j k) seg) seg) seg end;
