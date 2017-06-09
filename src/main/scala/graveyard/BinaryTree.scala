// import scala.collection.mutable._

// // Binary tree ADT.
// abstract class BinaryTree
// case class Node (value : Int,
//                  left : BinaryTree, right : BinaryTree) extends BinaryTree
// case class Leaf (value : Int) extends BinaryTree

// // An obvious recursive solution. Can cause a stack overflow if the tree is
// // very tall.
// def inorder_recursive(t : BinaryTree) {
//   t match {
//     case Node(v, left, right) =>
//       inorder_recursive(left)
//       print("%d ".format(v))
//       inorder_recursive(right)
//     case Leaf(v) =>
//       print("%d ".format(v))
//   }
// }

// // An iterative solution that uses an explicit stack and will work even for
// // very tall trees. Both solutions need O(N) time and O(H) space, where N
// // is the size of the tree an H is the height of the tree.
// def inorder_iterative(t : BinaryTree) {
//   val st = Stack[BinaryTree]()
//   st.push(t)
//   while (!st.isEmpty) {
//     st.pop() match {
//       case Node(v, left, right) =>
//         st.push(right)
//         st.push(Leaf(v))
//         st.push(left)
//       case Leaf(v) =>
//         print("%d ".format(v))
//     }
//   }
// }

// // Program entry point.
// object Main {
//   def main(args : Array[String]) {
//     val t = Node(4, Node(2,Leaf(1),Leaf(3)), Node(6,Leaf(5),Leaf(7)));

//     println("inorder_recursive:")
//     inorder_recursive(t)
//     println("\ninorder_iterative:")
//     inorder_iterative(t)
//   }
// }