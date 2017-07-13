import scala.io.Source

case class Rectangle (minx: Int, maxx: Int, miny: Int, maxy: Int)


class Synmap(val filename: String) {
	//converts String numbers to Some(Double)
	def parseDouble(s: String) = try { Some(s.toDouble) } catch { case _  => None }

	//takes output of synmap and retrieves ks, xs, ys (as Lists)
	def getData() = {
		val src = Source.fromFile(filename) //BufferedSource
		val sm = src.getLines.toList //List[String] //or src.getLines.par
		val data = for (line <- sm if !line.startsWith("#")) yield line.replaceAll("\\|", " ").split("\\s+") //List[Array[String]]
		val keep = for ( d <- data if !d.contains("NA")) yield d //List[Array[String]]
		val coors = for (k <- keep) yield Array(parseDouble(k(0)), parseDouble(k(10)), parseDouble(k(22))) //
		//List[Option[Double]]

		val ks = ((for (points <- coors) yield points(0)).flatten) //List[Double]
		val xs = ((for (points <- coors) yield points(1)).flatten) map (_.toInt) //List[Int]
		val ys = ((for (points <- coors) yield points(2)).flatten) map (_.toInt)

		src.close

		(xs, ys, ks)

		}

	def synmapSize(xs: List[Int], ys: List[Int]) = {
		val global_minx = xs.min
		val global_maxx = xs.max
		val global_miny = ys.min
		val global_maxy = ys.max
		(global_minx, global_maxx, global_miny, global_maxy)
	}


	def closestSquare(n: Int): Int = {
		val n_sqrt = Math.sqrt(n)
		val n_rounded = Math.ceil(n_sqrt)
		val answer = Math.pow(n_rounded, 2)
		answer.toInt
	}

	def synmapRectangle(maxx: Int, maxy: Int): Rectangle = {
		val minx: Int = 0
		val miny: Int = 0
		val rounded_maxx = closestSquare(maxx)
		val rounded_maxy = closestSquare(maxy)
		val rec: Rectangle = Rectangle(minx, rounded_maxx, miny, rounded_maxy)
		rec
	}

}



val s: Synmap = new Synmap("/Users/heather/Desktop/matrixNgrams/src/main/scala/miniks1.txt")
val (xs, ys, ks) = s.getData()
val (minx, maxx, miny, maxy) = s.synmapSize(xs, ys)
val rec = s.synmapRectangle(maxx, maxy)




