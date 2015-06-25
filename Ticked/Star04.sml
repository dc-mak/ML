(* Dhruv Makwana, Trinity College, dcm41 *)

(* From Tick 4*)
use "Ex04.sml";

(* Draw a straight line *)
fun drawLine image color (x0, y0) (x1, y1) =
let
  fun step diff = (Int.abs diff, Int.sign diff)
  val (dx, sx) = step (x1-x0)
  val (dy, sy) = step (y1-y0)
  fun drawDot (x, y, err) =
  let
    val e2 = 2*err
    val (x', err) = if e2 > ~dy then (x + sx, err-dy) else (x, err)
    val (y', err) = if e2 <  dx then (y + sy, err+dx) else (y, err)
  in
    drawPixel image color (x,y);
    if x = x1 andalso y = y1 then () else drawDot (x', y', err)
  end
in
  drawDot (x0, y0, dx - dy)
end;

(* Background *)
val (dim as (w,h)) = (1920, 1080);
val saltire = image dim (0, 0, 150);
val white = (255, 255, 255);

(* Drawing multiple lines *)
val thick = w div 20;
fun zip [] ys = []
  | zip xs [] = []
  | zip (x::xs) (y::ys) = (x,y)::zip xs ys;

(* Top-left to bottom-right *)
fun place n = if n < thick then (0, n) else (n - thick + 1, 0);
val startTL = List.tabulate(2*thick - 1, place);

fun shift (x,y) = (w-1-y, h-1-x)
val finBR = map shift startTL;

val line1 = zip startTL finBR;

(* Top-right to bottom-left *)
fun flipX (x,y) = (w-1-x, y)
fun flipLine [] = []
  | flipLine ((p0, p1)::xs) = (flipX p0, flipX p1)::flipLine xs;

val line2 = flipLine line1;

(* Apply/draw lines onto image *)
fun drawCross (start, fin) = drawLine saltire white start fin;
List.app drawCross line1;
List.app drawCross line2;
toPPM saltire "Star04-Saltire.ppm";

(*
 * val drawLine = fn: 'a array array -> 'a -> int * int -> int * int -> unit
 * val dim = (1920, 1080): int * int
 * val h = 1080: int
 * val w = 1920: int
 * val saltire =
 *    fromList[fromList[(0, 0, 150), (0, 0, 150), (0, 0, 150), (0, 0, 150),
 *          (0, 0, 150), (0, 0, 150), (0, 0, 150), (0, 0, 150), (0, ...), ...],
 *       fromList[(0, 0, 150), (0, 0, 150), (0, 0, 150), (0, 0, 150),
 *          (0, 0, 150), (0, 0, 150), (0, 0, 150), (0, ...), ...],
 *       fromList[(0, 0, 150), (0, 0, 150), (0, 0, 150), (0, 0, 150),
 *          (0, 0, 150), (0, 0, 150), (0, ...), ...],
 *       fromList[(0, 0, 150), (0, 0, 150), (0, 0, 150), (0, 0, 150),
 *          (0, 0, 150), (0, ...), ...],
 *       fromList[(0, 0, 150), (0, 0, 150), (0, 0, 150), (0, 0, 150), (0, ...),
 *          ...],
 *       fromList[(0, 0, 150), (0, 0, 150), (0, 0, 150), (0, ...), ...],
 *       fromList[(0, 0, 150), (0, 0, 150), (0, ...), ...],
 *       fromList[(0, 0, 150), (0, ...), ...], fromList[(0, ...), ...],
 *       fromList[...], ...]: (int * int * int) array array
 * val white = (255, 255, 255): int * int * int
 * val thick = 96: int
 * val place = fn: int -> int * int
 * val pair = fn: int * int -> int * int
 * val zip = fn: 'a list -> 'b list -> ('a * 'b) list
 * val startTL =
 *    [(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8),
 *     (0, 9), ...]: (int * int) list
 * val finBR =
 *    [(1919, 1079), (1918, 1079), (1917, 1079), (1916, 1079), (1915, 1079),
 *     (1914, 1079), (1913, 1079), (1912, 1079), (1911, 1079), (1910, 1079),
 *     ...]: (int * int) list
 * val line1 =
 *    [((0, 0), (1919, 1079)), ((0, 1), (1918, 1079)), ((0, 2), (1917, 1079)),
 *     ((0, 3), (1916, 1079)), ((0, 4), (1915, 1079)), ((0, 5), (1914, 1079)),
 *     ((0, 6), (1913, 1079)), ((0, 7), (1912, 1079)), ((0, 8), (1911, 1079)),
 *     ((0, 9), (1910, 1079)), ...]: ((int * int) * (int * int)) list
 * val flip = fn:
 *    ((int * 'a) * (int * 'b)) list -> ((int * 'a) * (int * 'b)) list
 * val line2 =
 *    [((1919, 0), (0, 1079)), ((1919, 1), (1, 1079)), ((1919, 2), (2, 1079)),
 *     ((1919, 3), (3, 1079)), ((1919, 4), (4, 1079)), ((1919, 5), (5, 1079)),
 *     ((1919, 6), (6, 1079)), ((1919, 7), (7, 1079)), ((1919, 8), (8, 1079)),
 *     ((1919, 9), (9, 1079)), ...]: ((int * int) * (int * int)) list
 * val drawCross = fn: (int * int) * (int * int) -> unit
 * val it = (): unit
 * val it = (): unit
 * val it = (): unit
 * val it = (): unit
 *)
