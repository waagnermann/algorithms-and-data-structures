import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack

object TopologicalSort {
	class Graph(vertexNumber: Int) {
		private val adj = Array.fill(vertexNumber)(ArrayBuffer.empty[Int])

		def addEdge(vertexParent: Int, vertexChild: Int): Unit = {
			adj(vertexParent).append(vertexChild)
		}

		override def toString: String = adj.zipWithIndex flatMap {
			 case (vertexParent, index) => vertexParent map { vertexChild => (index, vertexChild) } 
		} mkString(", ")

		def TopologicalSortUtil(vertexParentIndex: Int, visited: Array[Boolean], stack: Stack[Int]): Unit = {
			if (!visited(vertexParentIndex)) {
				visited(vertexParentIndex) = true
				adj(vertexParentIndex) foreach (vertexChild => TopologicalSortUtil(vertexChild, visited, stack))
				stack.push(vertexParentIndex)
			}
		}

		def topologicalSort(): String = {
			val stack = Stack.empty[Int]
			val visited = Array.ofDim[Boolean](vertexNumber)

			adj.zipWithIndex foreach { case (_, index) => TopologicalSortUtil(index, visited, stack)}

			stack mkString ", "
		}
	}

	def main(args: Array[String]): Unit = {
		val graph: Graph = new Graph(6);
        graph.addEdge(5, 2) 
        graph.addEdge(5, 0) 
        graph.addEdge(4, 0) 
        graph.addEdge(4, 1) 
        graph.addEdge(2, 3) 
        graph.addEdge(3, 1)
        println("Graph: " + graph)
        println("TopologicalSort: " + graph.topologicalSort())
	}
}