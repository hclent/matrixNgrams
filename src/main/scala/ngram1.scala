import breeze.linalg._
import breeze.numerics._
import scala.collection.mutable.ArrayBuffer

class ngramMatrix {
	//Initialize empty dense matrix
	def makeDense(x: Int, y: Int): breeze.linalg.DenseMatrix[Double] = DenseMatrix.zeros[Double](x,y)

	// Fill dense matrix with random values
	def fillDense(Matrix: breeze.linalg.DenseMatrix[Double]): breeze.linalg.DenseMatrix[Double] = {
		val r = new scala.util.Random(100)
		for(i <-0 until Matrix.rows)
			for(j <- 0 until Matrix.cols)
				Matrix(i,j) = r.nextInt
		return Matrix
	}

	// Return list of matrix ngrams for dense matrix
	def ngrams4dense(Matrix: breeze.linalg.DenseMatrix[Double], n:Int): ArrayBuffer[breeze.linalg.DenseMatrix[Double]] = {
		val ngramsArr = ArrayBuffer[breeze.linalg.DenseMatrix[Double]]()
		for(i <-0 until Matrix.rows) // traverse rows, top down
			for(j <- 0 until Matrix.cols) // traverse columns, left to right
				try {
					ngramsArr += Matrix( 0+i until n+i, 0+j until n+j).copy
					//println(Matrix( 0+i until n+i, 0+j until n+j).copy)
					//println("------------------------------------------")
				} catch { //if there's not enough columns/rows left to have a nxn sub-matrix
					case _: Throwable => println("not enough rows/columns whatever")
				}
		return ngramsArr
	}

	//input: ngrams ArrayBuffer
	//output: diagonals for each ngram as a DenseVector stored in ArrayBuffer[DenseVector]
	// [1 0 0]  left2right DOWN diagonal
	// [0 2 0]
	// [0 0 3]
	def downDiagonal(ngrams: ArrayBuffer[breeze.linalg.DenseMatrix[Double]], n:Int): ArrayBuffer[breeze.linalg.DenseVector[Double]] = {
		val diagonalsArray = ArrayBuffer[breeze.linalg.DenseVector[Double]]()
		for (gram <- ngrams) { //for each gram in ngrams
			//println(gram)
			//println("~*~**~*~*~*~*~**~*~*~**~*~*~*")
			var diagonal = diag(gram) //get diagonal
			diagonalsArray += diagonal
		}
		return diagonalsArray
	}

	//downDiagonals can't be processed with averageUPDiagonals function (below) because returns different
	//data structure .... do this later....

	//input: ngrams ArrayBuffer
	//output: diagonals for each ngram as ArrayBuffer of ArrayBuffer[diagonal Doubles]
	// [0 0 3] left2right UP diagonal 
	// [0 2 0]
	// [1 0 0]
	def upDiagonal(ngrams: ArrayBuffer[breeze.linalg.DenseMatrix[Double]], n:Int): ArrayBuffer[ArrayBuffer[Double]] = {
		val diagonalsArray = ArrayBuffer[ArrayBuffer[Double]]()
		val range = n-1 to 0 by -1 
		for (gram <- ngrams) { //for each gram in ngrams
			var diag = ArrayBuffer[Double]() 
			for (i <- range) { //retrieve diagonals
				diag += gram(i, (n-1)-i) 
			}
			diagonalsArray += diag
		}
		return diagonalsArray
	}

	//get average for n-gram UP diagonals
	def averageUPDiagonals(diagonals: ArrayBuffer[ArrayBuffer[Double]], n:Int): ArrayBuffer[Double] = {

		val averageArray = ArrayBuffer[Double]()

		for (darray <- diagonals){ //every n columns, go down 1 row 
			var numerator = darray.sum
			var denominator = darray.length
			var average = numerator / denominator
			averageArray += average
		}
		return averageArray
	}

	def resultsMatrix(M: breeze.linalg.DenseMatrix[Double], averages: ArrayBuffer[Double], n:Int): breeze.linalg.DenseMatrix[Double] = {
		//make results matrix from dimensions of old matrix
		var dim = (M.rows, M.cols)
		var og_x = dim._1
		var og_y = dim._2

		var new_x = og_x - n + 1
		var new_y = og_y - n + 1
		//results matrix 
		var R: breeze.linalg.DenseMatrix[Double] = DenseMatrix.zeros[Double](new_x, new_y)

		var avgs = averages.toArray.toIterator

		for(i <- 0 until R.cols)
			for(j <- 0 until R.rows){
				R(i,j) = avgs.next()
			}

		return R
	}


}

// val inst: ngramMatrix = new ngramMatrix
// var M = DenseMatrix((0.0, 0.0, 1.0, 2.0, 5.0), (0.0, 0.0, 0.0, 2.0, 2.0), (0.0, 1.0, 3.0, 0.0, 1.0),(0.0, 4.0, 1.0, 0.0, 0.0),(5.0,0.0,0.0,0.0,0.0))
// println("Starting Matrix")
// println(M)
// println("------------------------")
// println(" N = 3 ")
// println("* Starting Ngram analysis .... ")
// println("------------------------")
// println("Ngrams, as ArrayBuffer of matricies: ")
// val ngrams = inst.ngrams4dense(M, 3)
// println(ngrams)
// println("------------------------")
// println("Extract upwards diagonals ... ")
// val updiags = inst.upDiagonal(ngrams,3)
// println(updiags)
// println("------------------------")
// println("Calculating averages of diagonals ....")
// var avgs = inst.averageUPDiagonals(updiags, 3)
// println(avgs)
// println("-------------------------")
// println("Results Matrix of averages: ")
// val R = inst.resultsMatrix(M, avgs, 3)
// println(R)
// println("done!")


// what are the operations on the matricies that we want to support?

// store sums like that 

// id iy rnyitrly indifr? outside?


