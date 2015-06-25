(* 2011 Paper 1, Q1 FoCS *)
datatype arrow = L | R | U | D;
exception OffGrid;

(* Just because arrows are easier than numbers *)
fun convert (x,y) dir = case dir of
                             L => (x-1, y)
                           | R => (x+1, y)
                           | U => (x, y+1)
                           | D => (x, y-1);
(* Can have other labyrinths, this one relies on adjacency lists *)
fun lab1 x = map (convert x) (case x of
                                   (0,0) => [U, R]
                                 | (1,0) => [L, R]
                                 | (2,0) => [L]
                                 | (3,0) => [U, R]
                                 | (4,0) => [U, L]
                                 | (0,1) => [U, D]
                                 | (1,1) => [R]
                                 | (2,1) => [L, R]
                                 | (3,1) => [L, U, D]
                                 | (4,1) => [D]
                                 | (0,2) => [R, D]
                                 | (1,2) => [L, R]
                                 | (2,2) => [L, R, U]
                                 | (3,2) => [L, D]
                                 | (4,2) => [U]
                                 | (0,3) => [R, U]
                                 | (1,3) => [L]
                                 | (2,3) => [U, D]
                                 | (3,3) => [U]
                                 | (4,3) => [U, D]
                                 | (0,4) => [R, D]
                                 | (1,4) => [L, R]
                                 | (2,4) => [L, D]
                                 | (3,4) => [R, D]
                                 | (4,4) => [L, D]
                                 | _     => raise OffGrid);

(* Pair equality *)
infix p_eq;
fun (x,y) p_eq (x',y') = (x = x') andalso (y = y');

infix isIn;
fun r' isIn [] = false
  | r' isIn (r::rs) = r p_eq r' orelse r' isIn rs;

(* List complementation *)
infix \\;
fun xs \\ ys =
let
  fun min [] qs r = r
    | min (p::ps) qs r =
    if p isIn qs then
      min ps qs r
    else
      min ps qs (p::r)
in
  min xs ys []
end;

(* Next possible moves in labyrinth *)
fun next lab (x,y) = lab (x,y);

(* Shortest path *)
fun spath lab p1 p2 =
let
  fun sp [] vis r = (vis, [])
    | sp (a::adj) vis r = 
    if p2 p_eq a then (vis, p2::r)
    (* Iterative deepening *)
    else case (sp ((next lab a) \\ vis) (a::vis) (a::r)) of
            (v,[]) => sp adj v r
          | (v, r) => (v, r)
in
  if (p1 p_eq p2) then [p1]
  else p1::rev (#2(sp (next lab p1) [] []))
end;
