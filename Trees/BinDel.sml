(*Binary Tree Deletion
---------------------- *)
datatype 'a tree = Lf |  Br of 'a * 'a tree * 'a tree;				

fun del (Lf, tree) = tree
 | 	del (tree, Lf)  = tree
 |	del (tree, Br((a,x), Lf, tar)) = Br((a,x), tree, tar)
 | 	del (tree, Br((a,x), Br((b,y), tbl, tbr), tar)) =
               Br((b,y), del(tree, tbl), Br((a,x), tbr, tar))

fun delete (Lf, node) = Lf
 |	delete (Br((a,x), tal, tar), node) =
	if node < a then 
      Br((a,x), delete (tal, node), tar)
	else if node > a
      then Br((a,x), tal, delete (tar, node))
	else (*node = a*)
      del (tal, tar);

(*Locate function*)
exception Empty of string;

fun locate(Lf, key) = raise Empty "Node"
 |	locate (Br((a,x), tl, tr), key) =
	if key > a then
      locate(tr, key)
	else if key < a then
      locate(tl, key)
	else (*key = a*) x;

val test = Br((4, "Stella"),
                Br((2, "James"),
                    Br((1, "Dhruv"), Lf, Lf), Br((3, "Milos"), Lf, Lf)),
                Br((6, "Anna"),
                    Br((5, "Andrej"), Lf, Lf), Br((7, "Gabor"), Lf, Lf)));

val ans = delete(test, 6);

(*Test trees. http://www.clittle.com/blog/ml-tree-generator *)

(*Unbalanced tree deletion *)
fun insroot (Lf, tree) = tree
 | 	insroot (tree, Lf) = tree
 |	insroot (Br((a,x), tal, Lf), tree) = Br((a,x), tal, tree)
 |	insroot (Br((a,x), tal, tar), tree) = Br((a,x), tal, insroot(tar, tree)); 
 
 fun ndelete (Lf, node) = Lf
 |	ndelete (Br((a,x), Lf, tar), node) = Br((a,x), Lf, ndelete(tar, node))
 |	ndelete (Br((a,x), Br((b,y), tbl, tbr), tar), node) =
	if node < a then
      Br((a,x), ndelete(Br((b,y), tbl, tbr), node), tar)
	else if node > a then
      Br((a,x), Br((b,y), tbl, tbr), ndelete(tar, node))
	else (*node = a*)
      Br((b,y), tbl, insroot(tbr, tar));
