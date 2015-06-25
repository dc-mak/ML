(* Dhruv Makwana, Trinity College, dcm41 *)

(* From Tick 4 *)
use "Ex04.sml";

(* Q1: It returns unit because it modifies the existing image in place. *)
fun drawAll f image = 
 appi (fn (y, row) => modifyi (fn (x, pixel) => f (x, y)) row) image;

(* Q2 *)
fun gradient (x,y) =
  (((x div 30) * 30) mod 256, 0, ((y div 30) div 30) mod 256);

fun gradImage () =
let
  val picture = image (640, 480) (0,0,0)
in
  drawAll gradient picture;
  toPPM picture "Ex05-Gradient.ppm"
end;

(* Q3 *)
fun mandelbrot maxIter (x,y) =
let
  fun solve (a,b) c =
    if c = maxIter then 1.0 else
      if (a*a + b*b <= 4.0) then
        solve (a*a - b*b + x, 2.0*a*b + y) (c+1)
      else
        (real c)/(real maxIter)
in
  solve (x,y) 0
end;

(* Q4 *)
fun chooseColor n =
let
  val r = round ((Math.cos n) * 255.0)
  val g = round ((Math.cos n) * 255.0)
  val b = round ((Math.sin n) * 255.0)
in
  (r, g, b)
end;

(* Q5 *)
fun rescale (w, h) (cx, cy, s) (x, y) = 
let
  val p = s * (real x / real w - 0.5) + cx
  val q = s * (0.5 - real y / real h) + cy
in 
  (p, q)
end;

(* Q6 *)
fun compute coords =
let
  val dim = (720, 720)
  fun paint xy = chooseColor (mandelbrot 100 (rescale dim coords xy))
  fun make pic = (drawAll paint pic; toPPM pic)
in
  make (image dim (0,0,0))
end;

(* Time spent on original program - 6:30 *)
gradImage();
compute (~0.5, 0.0, 2.0) "Ex05-Test.ppm";
compute (~0.74364990,0.13188204,0.00073801) "Ex05-Mandelbrot.ppm";

(*
 * val drawAll = fn: (int * int -> 'a) -> 'a array array -> unit
 * val gradient = fn: int * int -> int * int * int
 * val gradImage = fn: unit -> unit
 * val mandelbrot = fn: int -> real * real -> real
 * val chooseColor = fn: real -> int * int * int
 * val rescale = fn: int * int -> real * real * real -> int * int -> real * real
 * val compute = fn: real * real * real -> string -> unit
 * val it = (): unit
 * val it = (): unit
 * val it = (): unit
 * val it = (): unit
 *)
