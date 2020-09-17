object PerfectSquareRoot {
	
	def sqrt(n: Int): Int = {
		sqrtHelper(n, 1, n)
	}

	def sqrtHelper(n: Int, min: Int, max: Int): Int = {
		println(s"n: $n, min: $min, max: $max")
		if (max < min) {
			val res = -1
			res
		}
		else {
			val guess = (min + max) / 2
			if (guess * guess == n) guess
			else if (guess * guess < n) {
				sqrtHelper(n, guess + 1, max)
			}
			else {
				sqrtHelper(n, min, guess - 1)
			}
		}
	}

	def main(args: Array[String]): Unit = {
		val candidate = 65
		println(s"sqrt of $candidate: ${sqrt(candidate)}")
	}
}