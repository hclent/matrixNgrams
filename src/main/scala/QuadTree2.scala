case class Rectangle (minx: Int, maxx: Int, miny: Int, maxy: Int)

abstract class QuadTree(val rec: Rectangle)

case class Node (value : Int, nw : QuadTree, ne: QuadTree, sw: QuadTree, se: QuadTree, override val rec: Rectangle) extends QuadTree(rec)
case class Leaf (value : Int, override val rec: Rectangle) extends QuadTree(rec)
case class Empty (value: Int, override val rec: Rectangle) extends QuadTree(rec)


//Do two rectangles overlap?
//Basically the rectangles will not overlap if they don't share any of the same numbers in their ranges 
//NB: these are INCLUSIVE!!!  n                                                                    
def rectanglesOverlap(r1: Rectangle, r2:Rectangle): Boolean = {
  r2 match {
     //If the range of r1's x-axis does NOT contain anything from r2's x-axis, they don't overlap
     case x_overlap1 if (! (  ((r1.minx to r1.maxx) intersect (r2.minx to r2.maxx)).nonEmpty ) ) => false //where r1 is larger rectangle 
     case y_overlap1 if (! (  ((r1.miny to r1.maxy) intersect (r2.miny to r2.maxy)).nonEmpty ) ) => false
     //If the range of r2's x-axis does NOT contain anything from r1's x-axis, they don't overlap
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

// def example(foo: Int): Boolean = {
//   if (foo % 2 == 1) {
//     println("ODD!")
//     true
//   } else {
//     false
//   }
// }

def queryBoolean2(query: Rectangle, t: QuadTree): Boolean = {
  //Step 1: check to see if the query overlaps the QT
  if (rectangleInside(query, t.rec)) {
    println("Query entirely covers QT. Return True")
    t match {
      case Node(v, nw, ne, sw, se, rec) => true
      case Leaf(v, rec) => v != 0
      case Empty(v, rec) => false
    }
  } else {
    if (!rectanglesOverlap(query, t.rec) ) {
      println("Query not in QT!!")
      false
    } else {
      println("Query overlaps QT. Maybe matches... ")
      t match { //given a QT, check node or leaf
        case Node(v, nw, ne, sw, se, rec) => //if Node, check all nw, ne, and so on
          println("Node")
          queryBoolean2(query, nw) || queryBoolean2(query, ne) || queryBoolean2(query, sw) || queryBoolean2(query, se)
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
}




//Need to check if the qt is entirely inside the range 
def querySum(query: Rectangle, t: QuadTree): Int = {
  //Step 1: check to see if the query engulfs the entire QT
  if (rectangleInside(query, t.rec)) {
    println("Query entirely covers QT. Return value")
    t match {
      case Node(v, nw, ne, sw, se, rec) => v
      case Leaf(v, rec) => v
      case Empty(v, rec) => v
    }
  } else {
    //Step2: check to see if the query overlaps the QT at all
    if (!rectanglesOverlap(query, t.rec)) {
      println("doesn't overlap yo") //base case: doesn't overlap
      0
    } else { //else: it overlaps! 
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
    }
  }
}  





// val q1 = Rectangle(0, 15, 0, 15) 
// val q2 = Rectangle(0,8,9,17) //nw quadrant plus some stuff
// val q3 = Rectangle(20,30,20,30) //non-overlapping rectangle -- 0
// val q4 = Rectangle(0,16,0,16) //same size rectangle 
// val q5 = Rectangle(9,16,0,8) //se -- 1

// val tree2 = Node(13, 
//                 Node(12,
//                   Leaf(7,Rectangle(0,3,12,15)), 
//                   Leaf(3,Rectangle(4,7,12,15)),
//                   Leaf(2,Rectangle(0,3,8,11)), //changed ymax to 11 so it doesnt overlap with nw leaf
//                   Empty(0,Rectangle(4,7,8,11)), Rectangle(0,7,8,15)), //changed ymax to 11 so it doesn't overlap with ne leaf
//                 Empty(0,Rectangle(8,15,8,15)),
//                 Empty(0,Rectangle(0,7,0,7)),
//                 Leaf(1,Rectangle(8,15,0,7)), Rectangle(0,15,0,15))

