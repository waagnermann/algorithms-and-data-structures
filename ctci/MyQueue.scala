import scala.collection.mutable.Stack

class MyQueue {
	private val stackNewestOnTop = new Stack[Int]()
	private val stackOldestOnTop = new Stack[Int]()

	private def shiftStack: Unit = {
		if (stackOldestOnTop.isEmpty) {
			while (stackNewestOnTop.nonEmpty) {
				stackOldestOnTop.push(stackNewestOnTop.pop())
			}
		}
	}

	def enqueue(value: Int) = {
		stackNewestOnTop.push(value)
	}

	def peek(): Int = {
		shiftStack
		stackOldestOnTop.head
	}

	def dequeue(): Int = {
		shiftStack
		stackOldestOnTop.pop()
	}

}
object MyQueue {

	val queue = new MyQueue

	def main(args: Array[String]): Unit = {
		queue.enqueue(1)
		queue.enqueue(2)
		queue.enqueue(3)
		println(queue.dequeue)
		println(queue.dequeue)
		println(queue.dequeue)
	}
}