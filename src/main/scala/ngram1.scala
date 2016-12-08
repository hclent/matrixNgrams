import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable.ArrayBuffer


class ngramMatrix {
	//Initialize empty dense matrix
	def makeDense(x: Int, y: Int): breeze.linalg.DenseMatrix[Int] = DenseMatrix.zeros[Int](x,y)

	// Fill dense matrix with values
	def fillDense(Matrix: breeze.linalg.DenseMatrix[Int]): breeze.linalg.DenseMatrix[Int] = {
		val r = new scala.util.Random(100)
		for(i <-0 until Matrix.rows)
			for(j <- 0 until Matrix.cols)
				Matrix(i,j) = r.nextInt
		return Matrix
	}

	// Return list of matrix ngrams 
	def ngrams(Matrix: breeze.linalg.DenseMatrix[Int], n:Int): ArrayBuffer[breeze.linalg.DenseMatrix[Int]] = {
		val ngramsList = ArrayBuffer[breeze.linalg.DenseMatrix[Int]]()
		for(i <-0 until Matrix.rows)
			for(j <- 0 until Matrix.cols)
				try {
					ngramsList += Matrix( 0+i until n+i, 0+j until n+j).copy
					//println(Matrix( 0+i until n+i, 0+j until n+j).copy)
					//println("------------------------------------------")
				} catch { //if there's not enough columns/rows left to have a nxn sub-matrix
					case _: Throwable => println("not enough rows/columns whatever")
				}
				

		return ngramsList
	}
}

// val X = makeDense(120,130)
// val Y = fillDense(X)
// println(Y)
// val blah = ngrams(Y, 11) 
// //println(blah)
// println(blah.length)