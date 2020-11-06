import scala.collection.mutable.Queue

object ShortestPathProblem {
	class Graph(v: Int) {
		/* adj = array of lists of adjastements*/
		private val adj: Array[List[Int]] = Array.fill(v)(List.empty[Int])
		private val EdgeLength: Int = 10 // We can define any length of edge, but for now let`s assign it to 10

		def addEdge(from: Int, to: Int): Unit = adj(from) = to::adj(from)

		def shortestReach(origin: Int): String = {

			val distances: Array[Int] = Array.fill(v)(-1) // Assign all distances as -1 (or infinite) initially
			val queue: Queue[Int] = Queue.empty[Int]

			def setShorterDistance(currentNodeDistance: Int, neighbor: Int): Unit = {
				distances(neighbor) == -1 match {
					case false => 
					case true =>
						queue.enqueue(neighbor) 
						distances(neighbor) = currentNodeDistance + EdgeLength
				}
			}

			queue.enqueue(origin)
			distances(origin) = 0

			while (queue.nonEmpty) {
				val currentNode: Int = queue.dequeue
				val currentNodeNeighbors: List[Int] = adj(currentNode)
				val currentNodeDistance: Int = distances(currentNode)
				currentNodeNeighbors foreach { member => setShorterDistance(currentNodeDistance, member)}
			}

			distances.zipWithIndex map { 
				case (distance, index) => s"Distance from $origin to $index is: $distance" 
			} mkString ("[", "\n ", "]")
		}

		override def toString: String = adj.zipWithIndex flatMap { 
			case (list, from) => list map { case to => s"$from $to" } 
		 } mkString "\n"
	}

	object Graph {
		def apply(v: Int) = new Graph(v)
	}

	def main(args: Array[String]): Unit = {
		val graph = Graph(9)

  		graph.addEdge(0, 3) 
        graph.addEdge(0, 5) 
        graph.addEdge(1, 2) 
        graph.addEdge(1, 3) 
        graph.addEdge(3, 0) 
        graph.addEdge(3, 4)
        graph.addEdge(3, 6)
        graph.addEdge(4, 5)
        graph.addEdge(0, 5)
        graph.addEdge(7, 8)


        // println(graph.toString)
        println(graph.shortestReach(1))
	}
}