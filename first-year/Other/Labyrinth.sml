(* 2011 Paper 1, Q1 FoCS *)
datatype arrow = L | R | U | D
exception OffGrid

(* Just because arrows are easier than numbers *)
fun convert (x,y) dir =
    case dir of
         L => (x-1, y)
       | R => (x+1, y)
       | U => (x, y+1)
       | D => (x, y-1)

(* Can have other labyrinths, this one relies on adjacency lists *)
fun lab1 x = map (convert x)
  (case x of
        (0,0) => [U, R]     | (0,1) => [U, D]      | (0,2) => [R, D]
      | (1,0) => [L, R]     | (1,1) => [R]         | (1,2) => [L, R]
      | (2,0) => [L]        | (2,1) => [L, R]      | (2,2) => [L, R, U]
      | (3,0) => [U, R]     | (3,1) => [L, U, D]   | (3,2) => [L, D]
      | (4,0) => [U, L]     | (4,1) => [D]         | (4,2) => [U]

      | (0,3) => [R, U]     | (0,4) => [R, D]
      | (1,3) => [L]        | (1,4) => [L, R]
      | (2,3) => [U, D]     | (2,4) => [L, D]
      | (3,3) => [U]        | (3,4) => [R, D]
      | (4,3) => [U, D]     | (4,4) => [L, D]

      | _     => raise OffGrid)

infix 4 isIn
fun r' isIn []      = false
  | r' isIn (r::rs) = (r'=r) orelse r' isIn rs

(* List complementation *)
infix 5 \\
fun xs \\ ys = let

    fun minus ([], qs, r) = r
      | minus (p::ps, qs, r) =
        if p isIn qs then minus (ps, qs, r) else minus (ps, qs, p::r)

  in minus (xs, ys, []) end

(* Next function, not necessary in this case *)
fun next labr xy = labr xy

(* Checking to see if a path exists: depth first search *)
fun pathExists labr start fin = if start = fin then [start] else let

    fun path ([],     visited, soFar) = (visited, [])
      | path (a::adj, visited, soFar) =
          if a = fin then (visited, fin::soFar) else
          case path (labr a \\ visited, a::visited, a::soFar) of
                  (v, [])    => path (adj, v, soFar) (* Failure, so no path *)
                | (v, soFar) => (v, soFar)           (* Success, so return  *)

  in rev (#2(path (labr start, [start], [start]))) end

(* Iterative deepening: termination ensured by forcing to provide maxDepth *)
fun shortPath labr maxDepth start fin = if start = fin then [start] else let

    fun path _     ([],     visited, soFar) = (visited, [])
      | path 0     (adj,    visited, soFar) =
          if fin isIn adj then (visited, fin::soFar) else (visited, [])
      | path depth (a::adj, visited, soFar) =
          case path (depth-1) (labr a \\ visited, a::visited, a::soFar) of
                  (v, [])    => path depth (adj, v, soFar)
                | (v, soFar) => (v, soFar)

    val initial = (labr start, [start], [start])

    fun iterate 0 = rev (#2(path maxDepth initial))
      | iterate n =
        case path (maxDepth-n) initial of
             (v, [])      => iterate (n-1)
           | (v, revPath) => rev revPath

  in iterate maxDepth end
