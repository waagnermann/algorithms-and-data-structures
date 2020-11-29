import scala.collection.mutable.Map

object CountUniquePaths {
	def countPaths(grid: Array[Array[Boolean]], cached: Map[Tuple2[Int, Int], Int]): Int = {
		val (maxRow, maxCol): (Int, Int) = (grid.length - 1, grid(0).length - 1)

		def run(grid: Array[Array[Boolean]], row: Int, col: Int, maxRow: Int, maxCol: Int): Int = {
			if (nonValidPosition(grid, row, col, maxRow, maxCol)) 0 
			else if (row == maxRow && col == maxCol) 1
			else cached get (row, col) match {
				case Some(count) => 
					println(s"row: $row, col: $col (Some)")
					count
				case None => 
					println(s"row: $row, col: $col (None)")
					val downwardsCache: Int = run(grid, row + 1, col, maxRow, maxCol)
					val rightwardsCache: Int = run(grid, row, col + 1, maxRow, maxCol)
					val cache: Int = downwardsCache + rightwardsCache
					cached((row, col)) = cache
					//cache //use run(grid, row, col, maxRow, maxCol) to make caching more visible
					run(grid, row, col, maxRow, maxCol)
			}
		}

		run(grid, 0, 0, maxRow, maxCol)
	}

	

	def nonValidPosition(grid: Array[Array[Boolean]], row: Int, col: Int, maxRow: Int, maxCol: Int): Boolean = {
		if (row < 0 || row > maxRow || col < 0 || col > maxCol || !grid(row)(col)) true else false
	}

	def main(args: Array[String]): Unit = {
		val grid: Array[Array[Boolean]] = Array(
			Array(true, true, true, true, true, true, true, true),
			Array(true, true, false, true, true, true, false, true),
			Array(true, true, true, true, false, true, true, true),
			Array(false, true, false, true, true, false, true, true),
			Array(true, true, false, true, true, true, true, true),
			Array(true, true, true, false, false, true, false, true),
			Array(true, false, true, true, true, false, true, true),
			Array(true, true, true, true, true, true, true, true)
			)

		println("Unique paths count: " + countPaths(grid, Map.empty[Tuple2[Int, Int], Int]))
	}
}