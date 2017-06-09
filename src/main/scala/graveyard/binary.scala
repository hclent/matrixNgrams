// class Tree[+T]
// case class TreeLeaf[T](val elem: T) extends Tree[T]
// case class TreeNode[T](val left: Option[Tree[T]], val right: Option[Tree[T]]) extends Tree[T]

// val mytree = TreeNode[Double](Some(TreeLeaf(1.0)), Some(TreeLeaf(2.0)));
// val mytree2 = TreeNode[Double](Some(mytree), None);

// def printTree[T](t: Tree[T]) {
// 	t match {
// 		case TreeLeaf(v) => { print(v); print(" "); };
// 		case TreeNode(left, right) => {
// 			print("{ ");
// 			left match {
// 				case Some(t) => printTree(t);
// 				case None => { print("{}"); }
// 			}
// 			right match {
// 				case Some(t) => printTree(t);
// 				case None => { print("{}"); }
// 			}
// 			print(" } ");
// 		}
// 	}
// }

// printTree(mytree); println("");
// printTree(mytree2);