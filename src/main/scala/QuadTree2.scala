case class Rectangle (minx: Int, maxx: Int, miny: Int, maxy: Int)

abstract class QuadTree(val value: Int, val rec: Rectangle)

case class Node (override val value : Int, nw : QuadTree, ne: QuadTree, sw: QuadTree, se: QuadTree, override val rec: Rectangle) extends QuadTree(value, rec)
case class Leaf (override val value : Int, override val rec: Rectangle) extends QuadTree(value, rec)
case class Empty (override val value: Int, override val rec: Rectangle) extends QuadTree(value, rec)


//Do two rectangles overlap, where r1 overlaps r2 **OR** r2 overlaps r1
//Basically the rectangles will not overlap if they don't share any of the same numbers in their ranges  
def rectanglesOverlap(r1: Rectangle, r2:Rectangle): Boolean = {
  r2 match {
     //In English: if r2's minx OR miny are not anywhere in the range of r1's x-axis, then there's no overlap along the x-axis
     //If the range of r1's x-axis does NOT contain anything from r2's x-axis, they don't overlap
     case x_overlap1 if (! (  (r1.minx to r1.maxx) intersect (r2.minx to r2.maxx) ) ) => false //where r1 is larger rectangle 
     case y_overlap1 if (! (  (r1.miny to r1.maxy) intersect (r2.miny to r2.maxy) ) ) => false
     //If the range of r2's x-axis does NOT contain anything from r1's x-axis, they don't overlap
     case x_overlap2 if (! (  (r2.minx to r2.maxx) intersect (r1.minx to r1.maxx)) ) => false //where r2 is larger rectangle
     case y_overlap2 if (! (  (r2.miny to r2.maxy) intersect (r1.miny to r1.maxy) ) ) => false
     case _ => true
  }
}
// BUG: scala> rectanglesOverlap(q1, q2)
// res0: Boolean = false
// Is false because q2 y-axis is 8-16, and neither q1 miny 1 or q1 maxy 16 is in that range, but clearly there is still overlap~
// To fix? rather than .contains minx||maxx, we need it to be .contains(anything between minx to maxx)



//Q: Is r2 is ENTIRELY inside r1? (r1 is probably a query that is larger than a node/leaf, after overlap)
def rectangleInside(r1: Rectangle, r2: Rectangle): Boolean = { 
  r2 match {
    case x_overlap1 if (! (  (r1.minx to r1.maxx).contains(r2.minx) && (r1.minx to r1.maxx).contains(r2.maxx) ) ) => false 
    case y_overlap1 if (! (  (r1.miny to r1.maxy).contains(r2.miny) && (r1.miny to r1.maxy).contains(r2.maxy) ) ) => false
    case _ => true
  }
}



def queryBoolean(query: Rectangle, t: QuadTree): Boolean = {
  //Step 1: check to see if the query overlaps the QT
  if (!rectanglesOverlap(query, t.rec) ) {
    println("Query not in QT!!")
    false
  } else {
      println("Query overlaps QT")
      t match { //given a QT, check node or leaf
        case Node(v, nw, ne, sw, se, rec) => //if Node, check all nw, ne, and so on
          println("Node")
          queryBoolean(query, nw)
          queryBoolean(query, ne)
          queryBoolean(query, sw)
          queryBoolean(query, se)
          true 
        case Leaf(v, rec) => //if leaf
          println("Leaf") 
          if (v!= 0) {   //check to see that the leaf isn't empty/numm
            true 
          } else {
            false
          }
        case Empty(v, rec) => 
          println("Empty")
          false
      }
    } 
  }





//Need to check if the qt is entirely inside the range 
def querySum(query: Rectangle, t: QuadTree): Int = {
  //Step 1: check to see if the query engulfs the entire QT
  // breakable {
  // if (rectangleInside(query, t.rec)) {
  //     println("Query entirely covers QT. Return value")
  //     t match {
  //       case Node(v, nw, ne, sw, se, rec) => v
  //       case Leaf(v, rec) => v
  //       case Empty(v, rec) => v
  //     }
  //     println("BREAK")
  //     break
  //   }
  // }
  //Step2: check to see if the query overlaps the QT at all
  if (!rectangleInside(query, t.rec)) {
    println("doesn't overlap yo")
    0
  } else {
    println("Query overlaps QT")
    // check for Node, Leaf, empty
    t match {
      case Node(v, nw, ne, sw, se, rec) =>
        println("Node")
        querySum(query, nw) + querySum(query, ne) + querySum(query, sw)+ querySum(query, se)
      case Leaf(v, rec) => 
        println("LEAF")
        v
      case Empty(v, rec) => 
        println("EMPTY")
        0
    }
  // Step3 : query doesn't overlapp QT, return 0
  } 
}





val q1 = Rectangle(1, 18, 1, 18) 
val q2 = Rectangle(1,8,8,16) 
val q3 = Rectangle(20,30,20,30) //non-overlapping rectangle -- 0
val q4 = Rectangle(1,16,1,16) //same size rectangle 
val q5 = Rectangle(8,16,1,8) //se -- 1
val tree1 = Node(13, 
                Node(12,
                  Leaf(7,Rectangle(1,4,12,16)),
                  Leaf(3,Rectangle(4,8,12,16)),
                  Leaf(2,Rectangle(1,4,8,12)),
                  Empty(0,Rectangle(4,8,8,12)), Rectangle(1,8,8,16)),
                Empty(0,Rectangle(8,16,8,16)),
                Empty(0,Rectangle(1,8,1,8)),
                Leaf(1,Rectangle(8,16,1,8)), Rectangle(1,16,1,16))



// Reads tree recursively
// Can crash on tall trees
// def inorder_recursive(t : QuadTree) {
//   t match {
//     case Node(v, nw, ne, sw, se) =>
//       print("Node: ")
//       print("%d ".format(v))
//       inorder_recursive(nw)
//       inorder_recursive(ne)
//       inorder_recursive(sw)
//       inorder_recursive(se)

//     case Leaf(v) =>
//       print("Leaf: ")
//       print("%d ".format(v))
//   }
// }

// //Reads tree iteravely. Memory efficient
// def inorder_iterative(t : QuadTree) {
//   val st = Stack[QuadTree]()
//   st.push(t)
//   while (!st.isEmpty) {
//     st.pop() match {
//       case Node(v, nw, ne, sw, se) =>
//         print("Node: ")
//         st.push(nw)
//         st.push(ne)
//         st.push(sw)
//         st.push(se)
//         st.push(Leaf(v))
//       case Leaf(v) =>
//         print("Leaf: ")
//         print("%d ".format(v))
//     }
//   }
// }

// // Program entry point.
// object Main {
//   def main(query: Range) {
    
//     val t = Node(13,
//                   Node(12,
//                     Leaf(7,Rectangle(1,4,12,16)),
//                     Leaf(3,Rectangle(4,8,12,16)),
//                     Leaf(2,Rectangle(1,4,8,12)),
//                     Empty(0,Rectangle(4,8,8,12)),Rectangle(1,8,8,16)),
//                   Empty(0,Rectangle(8,16,8,16)),
//                   Empty(0,Rectangle(1,8,1,8)),
//                   Leaf(1,Rectangle(8,16,1,8)), Rectangle(1,16,1,16))

//     println("inorder_recursive:")
//     inorder_recursive(t)
//     println("\ninorder_iterative:")
//     inorder_iterative(t)ßß
//   }
// }

//Main.main(Array(1,2,3,4))


//val emptyTree = Node(13, Empty(0, Rectangle(1, 8, 8, 16)), Empty(0, Rectangle(8, 16, 8, 16)),  Empty(0, Rectangle(1, 8, 1, 8)), Empty(0, Rectangle(8, 16, 1, 8)), Rectangle(1, 16, 1, 16))
//val tree1 = Node(13,Node(12,Leaf(7,Rectangle(1,4,12,16)),Leaf(3,Rectangle(4,8,12,16)),Leaf(2,Rectangle(1,4,8,12)),Empty(0,Rectangle(4,8,8,12)),Rectangle(1,8,8,16)),Empty(0,Rectangle(8,16,8,16)),Empty(0,Rectangle(1,8,1,8)),Leaf(1,Rectangle(8,16,1,8)),Rectangle(1,16,1,16))
