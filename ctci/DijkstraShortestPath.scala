object Dijkstra {
	private final val V = 9

	private def minDistance(dist: Array[Int], spt: Array[Boolean]): Int = {
		var (min, minIndex) = (Int.MaxValue, -1)

		((dist zip spt).zipWithIndex).foreach { 
			case ((distance, isIncluded), index) =>
				if (distance <= min && !isIncluded) {
					min = distance
					minIndex = index
				}
		}

		minIndex
	}

	private def printSolution(dist: Array[Int]): Unit = {
		println("Vertex \t\t Length of minimal path from Source")
		dist.zipWithIndex.foreach { case (distance, vertex) => println(s"$vertex \t\t $distance") }
	}

	def dijkstra(graph: Array[Array[Int]], source: Int): Unit = {
		val dist: Array[Int] = Array.fill(V)(Int.MaxValue)
		val spt: Array[Boolean] = Array.fill(V)(false)

		dist(source) = 0

		List.range(0, V - 1).foreach { _ => 
			val u: Int = minDistance(dist, spt)
			spt(u) = true

			List.range(0, V).foreach { v => if (!spt(v) && graph(u)(v) != 0 && dist(u) + graph(u)(v) < dist(v)) {
					dist(v) = dist(u) + graph(u)(v)
				}
			}
		}

		printSolution(dist)
	}

	def main(args: Array[String]): Unit = {
		val graph = Array(Array( 0, 4, 0, 0, 0, 0, 0, 8, 0 ), 
                          Array( 4, 0, 8, 0, 0, 0, 0, 11, 0 ), 
                          Array( 0, 8, 0, 7, 0, 4, 0, 0, 2 ), 
                          Array( 0, 0, 7, 0, 9, 14, 0, 0, 0 ), 
                          Array( 0, 0, 0, 9, 0, 10, 0, 0, 0 ), 
                          Array( 0, 0, 4, 14, 10, 0, 2, 0, 0 ), 
                          Array( 0, 0, 0, 0, 0, 2, 0, 1, 6 ), 
                          Array( 8, 11, 0, 0, 0, 0, 1, 0, 7 ), 
                          Array( 0, 0, 2, 0, 0, 0, 6, 7, 0 ))

		dijkstra(graph, 0)
	}
}