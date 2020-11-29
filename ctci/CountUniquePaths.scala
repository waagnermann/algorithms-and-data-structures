object CountUniquePaths {
	def countPaths(grid: Array[Array[Boolean]], row: Int, col: Int,
	 maxRow: Int, maxCol: Int, cached: Map[Tuple2[Int, Int], Int]): Int = {
	
		if (nonValidPosition(grid, row, col, maxRow, maxCol)) 0 
		else if (row == maxRow && col == maxCol) 1
		else cached get (row, col) match {
			case Some(count) => count
			case None => 
				val downwardsCache: Int = countPaths(grid, row + 1, col, maxRow, maxCol, cached)
				val rightwardsCache: Int = countPaths(grid, row, col + 1, maxRow, maxCol, cached)
				val cache: Int = downwardsCache + rightwardsCache
				countPaths(grid, row, col, maxRow, maxCol, cached updated ((row, col), cache))
		}
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

		val (maxRow, maxCol): (Int, Int) = (grid.length - 1, grid(0).length - 1)
		println("Unique paths count: " + countPaths(grid, 0, 0, maxRow, maxCol, Map.empty[Tuple2[Int, Int], Int]))
	}
}