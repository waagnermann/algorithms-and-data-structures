import scala.math.max

object ConnectedCellsProblem {

	def getBiggestRegion(matrix: Array[Array[Int]]): Int = {
		
		var biggestRegion: Int = 0
		val (length, width) = (matrix.length, matrix(0).length)
		
		def dfsRecursive(row: Int, column: Int): Int = {
			if (row < 0 || column < 0 || row >= length || column >= width) {
				val result = 0
				result
			}
			else if (matrix(row)(column) == 0) {
				val result = 0
				result
			}
			else {	
				var size = 1
				matrix(row)(column) = 0
				(row - 1 to row + 1) foreach {
					i => (column - 1 to column + 1) foreach {
						j => if (i != row || j != column) size += dfsRecursive(i, j)
					}
				}
				size
			}
		}
		
		(0 until length) foreach { 
			i => (0 until width) foreach {
				j => 
					val regionSize: Int = dfsRecursive(i, j)
					biggestRegion = max(biggestRegion, regionSize)
			}
		}

		biggestRegion
	}

	def main(args: Array[String]): Unit = {
		val matrix: Array[Array[Int]] = Array(Array(0, 0, 0, 1, 1, 0, 0),
											  Array(0, 1, 0, 0, 1, 1, 0),
											  Array(1, 1, 0, 1, 0, 0, 1),
											  Array(0, 0, 0, 0, 0, 1, 0),
											  Array(1, 1, 0, 0, 0, 0, 0),
											  Array(0, 0, 0, 1, 0, 0, 0))

		// println(matrix map(_.mkString("[", ", ", "]")) mkString ("[", ",\n ", "]"))

		println("The biggest region contains " + getBiggestRegion(matrix) + " cells")
	}
}