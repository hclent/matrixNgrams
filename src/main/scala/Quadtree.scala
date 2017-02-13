import breeze.linalg._

// val points: Array[Double] = Array(4.3611, 0.1999, 0.1549, 0.1066, 0.11)
// val xs: Array[Double] = Array(3.0, 4.0, 6.0, 11.0, 11.0)
// val ys: Array[Double] = Array(1.0, 2.0, 8.0, 10.0, 13.0)

// val points: Array[Double] = Array(4.3611, 0.1999, 0.1549, 0.1066, 0.11, 0.2629, 0.1389, 0.1624, 0.1959, 0.3495, 0.1416, 0.1748, 0.4111, 0.1504, 0.0726, 0.1514, 0.0565, 0.1517, 0.0969, 0.1134, 0.1184, 0.107, 0.1019, 0.0946, 0.1858, 0.1301, 0.0859, 0.1392, 0.1051, 0.6294, 0.2056, 0.1294, 0.1174, 0.0817, 0.1499, 0.1076, 0.123, 0.1655, 0.1124, 0.0747, 0.104, 0.1103, 0.0604, 0.0811, 0.1161, 0.1903, 0.1002, 0.1558, 0.1302, 0.1933, 0.5275, 0.0899, 0.101, 0.1163, 0.1168, 0.0989, 0.0833, 0.0952, 0.0813, 29.437, 0.1556, 0.102, 0.1046, 0.1245, 0.1372, 0.139, 0.1361, 0.1224, 0.141, 0.0703, 0.1485, 0.0713, 0.0901, 0.1228, 0.111, 0.2046, 0.107, 0.1179, 0.0952, 0.1248, 0.1253, 0.0643, 0.0696, 0.1171, 0.2105, 0.1515, 0.0785, 0.1131, 0.1248, 0.1227, 0.1962, 0.1385, 0.1171, 0.0907, 0.1013, 0.1452, 0.1128, 0.11)
// val xs: Array[Double] = Array(140.0, 144.0, 145.0, 146.0, 147.0, 148.0, 150.0, 151.0, 152.0, 153.0, 156.0, 157.0, 159.0, 160.0, 161.0, 162.0, 163.0, 164.0, 165.0, 166.0, 167.0, 168.0, 170.0, 171.0, 172.0, 173.0, 174.0, 175.0, 176.0, 177.0, 178.0, 180.0, 181.0, 182.0, 183.0, 184.0, 188.0, 189.0, 190.0, 191.0, 195.0, 196.0, 197.0, 198.0, 199.0, 200.0, 201.0, 202.0, 203.0, 204.0, 207.0, 208.0, 209.0, 210.0, 212.0, 215.0, 216.0, 217.0, 221.0, 224.0, 226.0, 227.0, 228.0, 229.0, 230.0, 231.0, 232.0, 233.0, 234.0, 235.0, 236.0, 237.0, 238.0, 239.0, 240.0, 241.0, 242.0, 243.0, 244.0, 246.0, 247.0, 248.0, 250.0, 251.0, 254.0, 255.0, 256.0, 257.0, 259.0, 261.0, 262.0, 263.0, 264.0, 265.0, 266.0, 268.0, 269.0, 270.0)
// val ys: Array[Double] = Array(173.0, 176.0, 177.0, 178.0, 180.0, 182.0, 183.0, 185.0, 186.0, 187.0, 189.0, 190.0, 191.0, 192.0, 194.0, 195.0, 196.0, 197.0, 199.0, 200.0, 201.0, 202.0, 207.0, 208.0, 210.0, 212.0, 213.0, 214.0, 215.0, 217.0, 218.0, 219.0, 221.0, 222.0, 223.0, 225.0, 226.0, 228.0, 229.0, 230.0, 233.0, 234.0, 236.0, 237.0, 238.0, 239.0, 240.0, 241.0, 242.0, 243.0, 245.0, 247.0, 252.0, 254.0, 257.0, 259.0, 261.0, 262.0, 266.0, 270.0, 275.0, 276.0, 278.0, 279.0, 280.0, 282.0, 283.0, 284.0, 285.0, 287.0, 288.0, 289.0, 290.0, 293.0, 295.0, 296.0, 297.0, 298.0, 299.0, 300.0, 302.0, 303.0, 305.0, 308.0, 310.0, 312.0, 313.0, 314.0, 315.0, 316.0, 317.0, 318.0, 320.0, 321.0, 322.0, 323.0, 324.0, 325.0)


class Node(val points: Array[Double], val xs: Array[Double], val ys: Array[Double]) {

	val maxPoints = 2.0 //maximum number of points per node (measure detail)

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
		println("local minx, miny, etc")
		println("0")
		println(matrix.rows -1) //maxx
		println("0")
		println(matrix.cols -1) //maxy

		var minx = 1
		var miny = 1
		var maxx = matrix.rows -1
		var maxy = matrix.cols - 1


		var cx = matrix.rows.toInt / 2
		var cy = matrix.rows.toInt / 2

		val maxPoints = 3 //measure Detail

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

				var children_w_sum: Array[Any] = Array(children, sum) //Array(Array[matricies], sum)
				
	

			if (num_points <= maxPoints) { //if there are maxPoints or less points, its a leaf! 
				println("there are maxPoints or fewer points! We have a leaf!")
				var p_val = givePointValue(matrix) //Array[Double]
				var leaf_w_sum: Array[Any] = Array(p_val, p_val.sum)

			}

			
		} else {
			println("there are no data points at all")
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
	println(minx)
	println(maxx)
	println(miny)
	println(maxy)

	val n = new Node(points, xs, ys)
	//should store root
	val root = n.makeRoot(points, xs, ys, maxx, maxy) //matrix 
	println(root)
	println("----------------------------")

	var depth = 0 

	def makeTree(matrix: DenseMatrix[Double]) {
		n.subdivide(matrix)
		//first division on root will return 4 children
		//then recurse on children
		//children will have different maxx, maxy, minx, miny 
		//(and remember that I added an extra row+column to init root matrix)

	}

}


val q: QuadTree = new QuadTree(points, xs, ys)
q.makeTree(q.root)






//print statements for children
// println("----- nw -----")
// println(nw)
// println("----- ne ------")
// println(ne)
// println("-------------------")
// println("---- sw ------ ")
// println(sw)
// println("----- se ------")
// println(se)
// println(nw.rows + sw.rows)
// println(ne.rows + se.rows )
// println(nw.cols + ne.cols)
// println(sw.cols + se.cols)
