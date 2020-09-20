import scala.collection.mutable.Stack

class MyQueue {
	private val stackNewestOnTop = new Stack[Int]()
	private val stackOldestOnTop = new Stack[Int]()

	def enqueue(value: Int) = {
		stackNewestOnTop.push(value)
	}

	def peek() = {
		if (stackOldestOnTop.nonEmpty) {
			stackOldestOnTop.head
		}
		else {
			while (stackNewestOnTop.nonEmpty) {
				stackOldestOnTop.push(stackNewestOnTop.pop())
			}
			stackOldestOnTop.head
		}
	}

	def dequeue() = {
		if (stackOldestOnTop.nonEmpty) {
			stackOldestOnTop.pop()
		}
		else {
			while (stackNewestOnTop.nonEmpty) {
				stackOldestOnTop.push(stackNewestOnTop.pop())
			}
			stackOldestOnTop.pop()
		}
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