import scala.collection.mutable.ArrayBuffer

object TopologicalSort {
	class Graph(vertexNumber: Int) {
		private val adj = Array.fill(vertexNumber)(ArrayBuffer.empty[Int])

		def addEdge(vertexParent: Int, vertexChild: Int): Unit = {
			adj(vertexParent).addOne(vertexChild)
		}

		override def toString: String = adj.zipWithIndex flatMap {
			 case (vertexParent, index) => vertexParent map { vertexChild => (index, vertexChild) } 
			} mkString(", ")
	}

	def main(args: Array[String]): Unit = {
		val graph: Graph = new Graph(6);
        graph.addEdge(5, 2) 
        graph.addEdge(5, 0) 
        graph.addEdge(4, 0) 
        graph.addEdge(4, 1) 
        graph.addEdge(2, 3) 
        graph.addEdge(3, 1)
        println(graph)
	}
}