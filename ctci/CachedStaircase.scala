import scala.collection.mutable.ArrayBuffer

object CachedStaircase {
	/*Count ways to reach the n’th stair, linear-time solution*/

	/*w(n) = fib(n - 1)*/
	def fib(n: Int): Int = {
		val cachedWindow = ArrayBuffer(1, 1)
		var i = 1
		while (i < n) {
			val previousLast = cachedWindow(1)
			val newLast = cachedWindow.sum
			cachedWindow(0) = previousLast
			cachedWindow(1) = newLast
			i += 1
		}
		cachedWindow(1)
	}

	def w(n: Int): Int = {
		require(n > 1 == true)
		fib(n - 1)
	}

	def main(args: Array[String]): Unit = {
		println(w(5))
	}
}