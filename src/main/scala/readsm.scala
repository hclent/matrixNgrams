import scala.io.Source
//import ai.lum.common.IteratorUtils._

class Preprocess {
	//converts String numbers to Some(Double)
	def parseDouble(s: String) = try { Some(s.toDouble) } catch { case _  => None }

	//takes output of synmap and retrieves ks, x, y
	def readFile(filename: String) = {
		val src = Source.fromFile(filename) //BufferedSource
		val sm = src.getLines.toList //List[String] //or src.getLines.par
		val data = for (line <- sm if !line.startsWith("#")) yield line.replaceAll("\\|", " ").split("\\s+") //List[Array[String]]
		val keep = for ( d <- data if !d.contains("NA")) yield d //List[Array[String]]
		val coors = for (k <- keep) yield Array(parseDouble(k(0)), parseDouble(k(10)), parseDouble(k(22))) //
		//List[Option[Double]]

		//these should actually go to Arrays, not lists 
		val ks = for (points <- coors) yield points(0)
		val x = for (points <- coors) yield points(1)
		val y = for (points <- coors) yield points(2)

		src.close
		//return tuple with ks, x, y where each is Array[Double]
		(ks.flatten.toArray, x.flatten.toArray, y.flatten.toArray)	
		}

}




// val inst: Preprocess = new Preprocess
// val synmap = inst.readFile("/Users/hclent/Desktop/matrixNgrams/src/main/scala/ks.txt")