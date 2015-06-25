(* Dhruv Makwana, Trinity College, dcm41 *)
open Array;

type color = int * int * int;
type xy = int * int;
type image = color array array;

(* Q1 *)
(* Create image with supplied dimensions and filled with color. *)
fun image (width, height) color =
    tabulate(height, fn i => tabulate(width, fn j => color));

(* Returns dimensions of image. *)
fun size image = (length (sub (image, 0)), length image);

(* Changes the colour of a pixel in an image. *)
fun drawPixel image col (x, y) = update(sub(image, y), x, col);

(* Q2 *)
fun toPPM image filename =
let
  fun format4 n = StringCvt.padLeft #" " 4 (Int.toString n)
  fun fmtPixel (r, g, b) = (format4 r)^(format4 g)^(format4 b)
  val (w, h) = size image
  (* File IO *)
  val oc = TextIO.openOut filename
  val header = "P3\n"^Int.toString w^" "^Int.toString h^"\n255\n"
  fun writePixels (pixel, row) = row^fmtPixel pixel
  fun writeRows (row, file) = file^foldl writePixels "" row^"\n"
  val file = header^foldl writeRows "" image
in
  TextIO.output (oc, file);
  TextIO.closeOut oc
end;

(* Q3 *)
fun colorRow image color row = modify (fn y => color) (sub(image,row));
fun colorCol image color col = app (fn x => update(x,col,color)) image;

(* Creating an image *)
val red = (255, 0, 0);
val green = (0, 255, 0);
val blue = (0, 0, 255);
val picture = image (30, 20) red;
colorRow picture green 10;
colorCol picture blue 15;
toPPM picture "Ex04-Picture.ppm";

(*
 * type color = int * int * int
 * type xy = int * int
 * type image = color array array
 * val image = fn: int * int -> 'a -> 'a array array
 * val size = fn: 'a array array -> int * int
 * val drawPixel = fn: 'a array array -> 'a -> int * int -> unit
 * val toPPM = fn: (int * int * int) array array -> string -> unit
 * val colorRow = fn: 'a array array -> 'a -> int -> unit
 * val colorCol = fn: 'a array array -> 'a -> int -> unit
 * val red = (255, 0, 0): int * int * int
 * val green = (0, 255, 0): int * int * int
 * val blue = (0, 0, 255): int * int * int
 * val picture =
 *    fromList[fromList[(255, 0, 0), (255, 0, 0), (255, 0, 0), (255, 0, 0),
 *          (255, 0, 0), (255, 0, 0), (255, 0, 0), (255, 0, 0), (255, ...), ...],
 *       fromList[(255, 0, 0), (255, 0, 0), (255, 0, 0), (255, 0, 0),
 *          (255, 0, 0), (255, 0, 0), (255, 0, 0), (255, ...), ...],
 *       fromList[(255, 0, 0), (255, 0, 0), (255, 0, 0), (255, 0, 0),
 *          (255, 0, 0), (255, 0, 0), (255, ...), ...],
 *       fromList[(255, 0, 0), (255, 0, 0), (255, 0, 0), (255, 0, 0),
 *          (255, 0, 0), (255, ...), ...],
 *       fromList[(255, 0, 0), (255, 0, 0), (255, 0, 0), (255, 0, 0),
 *          (255, ...), ...],
 *       fromList[(255, 0, 0), (255, 0, 0), (255, 0, 0), (255, ...), ...],
 *       fromList[(255, 0, 0), (255, 0, 0), (255, ...), ...],
 *       fromList[(255, 0, 0), (255, ...), ...], fromList[(255, ...), ...],
 *       fromList[...], ...]: (int * int * int) array array
 * val it = (): unit
 * val it = (): unit
 * val it = (): unit
 * val it = (): unit
 *)
