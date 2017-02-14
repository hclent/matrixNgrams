import breeze.linalg._

// val points: Array[Double] = Array(4.3611, 0.1999, 0.1549, 0.1066, 0.11)
// val xs: Array[Double] = Array(3.0, 4.0, 6.0, 11.0, 15.0)
// val ys: Array[Double] = Array(1.0, 2.0, 8.0, 10.0, 15.0)


class Node(val points: Array[Double], val xs: Array[Double], val ys: Array[Double]) {

	//val maxPoints = 2.0 //maximum number of points per node (measure detail)

	def makeRoot(points: Array[Double], xs: Array[Double], ys:Array[Double], maxx: Double, maxy:Double): DenseMatrix[Double] = {
		val empty_root: DenseMatrix[Double] = DenseMatrix.zeros[Double](maxx.toInt + 1, maxy.toInt + 1)
		for (i <- 0 until xs.length) {
			empty_root.update(xs(i).toInt, ys(i).toInt, points(i))
		}
		val root: DenseMatrix[Double] = empty_root
		root //is this better than a return statement or is this a return statement?
	}


	//access function
	//should be a boolean? maybe?
	//used to return the points in the node 
	def checkForData(matrix: DenseMatrix[Double]): Int = {
		var num_points = matrix.toDenseVector.toArray.filter(_ != 0.0).length
		num_points
	}

	//gets instance 
	def givePointValue(matrix: DenseMatrix[Double]): Array[Double] = {
		var p_val = matrix.toDenseVector.toArray.filter(_ != 0.0)
		p_val
	}


	//subdivide function (aka constructor) :
	//should be inside of Node class
	//should make  children
	//should be recursive 
	//should return type Node  
	def subdivide(matrix: DenseMatrix[Double]) {

		//TO DO: the minx, maxx, miny, maxy should be relative to the size of what's being put in, no?
		println("--------------- new subdivide call on a matrix -----------------")
		println("local minx, miny, etc")
		println("0")
		println(matrix.rows -1) //maxx
		println("0")
		println(matrix.cols -1) //maxy
		println("whole")
		println(matrix)

		var minx = 1
		var miny = 1
		var maxx = matrix.rows -1
		var maxy = matrix.cols - 1


		var cx = matrix.rows.toInt / 2
		var cy = matrix.rows.toInt / 2

		val maxPoints = 1 //measure Detail

		// FOR EACH CHILD MATRIX 
		// want to return (children, sum)
		if ( any(matrix) ) { //if there is any data in the matrix, 
			println("there are data points!")
			var num_points = checkForData(matrix)
			println(num_points)

			if (num_points > maxPoints) { //if there are more data points than we want, split into children
				println("there are more than maxPoints points in our Node matrix, so we need to split into children")
				var nw = matrix(minx.toInt-1 to cx.toInt, miny.toInt-1 to cy.toInt).copy
				var ne = matrix(minx.toInt-1 to cx.toInt , cy.toInt+1 to maxy.toInt).copy
				var sw = matrix(cx.toInt+1 to maxx.toInt, miny.toInt-1 to cy.toInt).copy
				var se = matrix(cx.toInt+1 to maxx.toInt, cy.toInt+1 to maxy.toInt).copy
				var children: Array[DenseMatrix[Double]] = Array(nw, ne, sw, se)

				println("----- nw -----")
				println(nw)
				println("---- sw ------ ")
				println(sw)
				println("-------------------")
				println("----- ne ------")
				println(ne)
				println("----- se ------")
				println(se)
				println(nw.rows + sw.rows)
				println(ne.rows + se.rows )
				println(nw.cols + ne.cols)
				println(sw.cols + se.cols)

				var c_sums = ( givePointValue(nw).sum +  givePointValue(ne).sum + givePointValue(sw).sum + givePointValue(se).sum   )
				println(" children sums: ")
				println( c_sums)

				//var children_w_sum: Array[Any] = Array(children, sum) //Array(Array[matricies], sum)
				for (c <- children) {
					subdivide(c)
				}
	

			if (num_points <= maxPoints) { //if there are maxPoints or less points, its a leaf! 
				println("there are maxPoints or fewer points! We have a leaf!")
				var p_val = givePointValue(matrix) //Array[Double]
				var leaf_w_sum: Array[Any] = Array(p_val, p_val.sum)

			}

			
		} else {
			println("-----")
			println("don't need to divide here ... ")
			//this will be used when we come to recursion, I suppose :x 
			var e_val: Array[Any] = Array(0.0, 0.0)
		
		}


		}

	
	}

}


//actuall makes quad tree using Node class
//stores root node and algorithms
class QuadTree(val points: Array[Double], val xs: Array[Double], val ys: Array[Double]) {
	val minx = xs.min
	val maxx = xs.max
	val miny = ys.min
	val maxy = ys.max

	val n = new Node(points, xs, ys)
	//should store root
	val root = n.makeRoot(points, xs, ys, maxx, maxy) //matrix 

	var depth = 0 

	def makeTree(matrix: DenseMatrix[Double]) {
		n.subdivide(matrix)
		//var barf = n.subdivide(matrix)
		//println(barf)
		//first division on root will return 4 children
		//then recurse on children
		//children will have different maxx, maxy, minx, miny 
		//(and remember that I added an extra row+column to init root matrix)

	}

}

// val q: QuadTree = new QuadTree(points, xs, ys)
// q.makeTree(q.root)