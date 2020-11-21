object RecursiveStaircase {
	/*Count ways to reach the n’th stair, exponential-time solution*/
	
	def w(n: Int): Int = {
		require(n > 0 == true)
		if (n == 2 || n == 1) 1
		else w(n - 1) + w(n - 2)
	}

	def main(args: Array[String]): Unit = {
		println(w())
	}
}